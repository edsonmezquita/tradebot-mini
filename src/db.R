box::use(
  DBI[dbConnect, dbDisconnect, dbExecute, dbGetQuery, dbWriteTable, dbIsValid, dbBegin, dbCommit, dbRollback],
  RPostgres[Postgres],
  data.table[as.data.table, setnames],
  uuid[UUIDgenerate],
  utils[URLdecode]
)

# ----------------------------------------------------------------------------
# Connection
# ----------------------------------------------------------------------------

.parse_pg_url <- function(url) {
  # postgres://user:pass@host:port/dbname?sslmode=require
  m <- regmatches(
    url,
    regexec("^postgres(?:ql)?://([^:]+):([^@]+)@([^:/]+):([0-9]+)/([^?]+)(?:\\?(.*))?$", url)
  )[[1]]
  if (length(m) < 6) {
    stop("Invalid DATABASE_URL — expected postgres://user:pass@host:port/dbname[?params]")
  }
  params <- list(
    user = URLdecode(m[2]),
    password = URLdecode(m[3]),
    host = m[4],
    port = as.integer(m[5]),
    dbname = m[6]
  )
  if (nzchar(m[7])) {
    qs <- strsplit(m[7], "&", fixed = TRUE)[[1]]
    for (kv in qs) {
      bits <- strsplit(kv, "=", fixed = TRUE)[[1]]
      if (length(bits) == 2L) params[[bits[1]]] <- bits[2]
    }
  }
  return(params)
}

# Per-process connection cache.
.conn_env <- new.env(parent = emptyenv())

#' Open (or reuse) the Postgres connection driven by DATABASE_URL.
#' @export
db_conn <- function() {
  c <- .conn_env$conn
  if (!is.null(c) && dbIsValid(c)) {
    return(c)
  }
  url <- Sys.getenv("DATABASE_URL")
  if (!nzchar(url)) {
    stop("DATABASE_URL not set")
  }
  p <- .parse_pg_url(url)
  conn <- dbConnect(
    Postgres(),
    host = p$host,
    port = p$port,
    dbname = p$dbname,
    user = p$user,
    password = p$password,
    sslmode = if (!is.null(p$sslmode)) p$sslmode else "require"
  )
  .conn_env$conn <- conn
  return(conn)
}

#' Close the cached connection (idempotent).
#' @export
db_disconnect <- function() {
  c <- .conn_env$conn
  if (!is.null(c) && dbIsValid(c)) {
    try(dbDisconnect(c), silent = TRUE)
  }
  .conn_env$conn <- NULL
  return(invisible())
}

# ----------------------------------------------------------------------------
# Migrations — runs every startup, idempotent CREATE TABLE IF NOT EXISTS.
# ----------------------------------------------------------------------------

#' Apply schema migrations. Walks db/migrations/*.sql in lexical order and
#' executes each. All DDL must be idempotent.
#' @export
db_migrate <- function(migrations_dir = "db/migrations") {
  files <- sort(list.files(migrations_dir, pattern = "\\.sql$", full.names = TRUE))
  if (length(files) == 0L) {
    cat("[db] no migration files in", migrations_dir, "\n")
    return(invisible())
  }
  conn <- db_conn()
  for (f in files) {
    cat("[db] applying", basename(f), "\n")
    raw <- paste(readLines(f, warn = FALSE), collapse = "\n")
    # Strip line comments, split on `;` at end of statement.
    stripped <- gsub("--[^\n]*", "", raw)
    stmts <- strsplit(stripped, ";\\s*(?=\\S|$)", perl = TRUE)[[1]]
    for (s in stmts) {
      s <- trimws(s)
      if (nzchar(s)) dbExecute(conn, s)
    }
  }
  return(invisible())
}

#' Generate a new cycle id (UUIDv4 string).
#' @export
new_cycle_id <- function() UUIDgenerate()

# ----------------------------------------------------------------------------
# memos
# ----------------------------------------------------------------------------

#' Insert one cycle memo row.
#' @export
memos_insert <- function(cycle_id, picks, buys, sells, holds, memo) {
  conn <- db_conn()
  dbExecute(
    conn,
    "INSERT INTO memos (cycle_id, picks, buys, sells, holds, memo)
     VALUES ($1, $2, $3, $4, $5, $6)",
    params = list(cycle_id, picks, buys, sells, holds, memo)
  )
  return(invisible())
}

#' Read memos, optionally filtered. Replaces the old CSV path.
#' @export
memos_read <- function(limit = 10L, order = "newest", ticker = NULL, since = NULL, until = NULL) {
  conn <- db_conn()
  where <- c()
  params <- list()
  i <- 1L
  if (!is.null(ticker) && nzchar(ticker)) {
    sym <- toupper(ticker)
    where <- c(where, sprintf("(picks ~* $%d OR buys ~* $%d OR sells ~* $%d OR holds ~* $%d)", i, i, i, i))
    params[[i]] <- sprintf("(^|,)%s(:|,|$)", sym)
    i <- i + 1L
  }
  if (!is.null(since) && nzchar(since)) {
    where <- c(where, sprintf("ts >= $%d::timestamptz", i))
    params[[i]] <- since
    i <- i + 1L
  }
  if (!is.null(until) && nzchar(until)) {
    where <- c(where, sprintf("ts <= $%d::timestamptz", i))
    params[[i]] <- until
    i <- i + 1L
  }
  where_clause <- if (length(where)) paste("WHERE", paste(where, collapse = " AND ")) else ""
  order_sql <- if (order == "oldest") "ts ASC" else "ts DESC"
  sql <- sprintf(
    "SELECT ts AS timestamp, picks, buys, sells, holds, memo
     FROM memos %s ORDER BY %s LIMIT %d",
    where_clause,
    order_sql,
    as.integer(limit)
  )
  if (length(params) > 0L) {
    return(as.data.table(dbGetQuery(conn, sql, params = params)))
  } else {
    return(as.data.table(dbGetQuery(conn, sql)))
  }
}

# ----------------------------------------------------------------------------
# twitter_state (singleton row, upsert)
# ----------------------------------------------------------------------------

#' @export
twitter_state_load <- function() {
  conn <- db_conn()
  rows <- dbGetQuery(conn, "SELECT * FROM twitter_state WHERE id = 1")
  if (nrow(rows) == 0L) {
    return(list())
  }
  return(list(
    user_id = rows$user_id,
    username = rows$username,
    last_mention_id = rows$last_mention_id,
    last_tweet_id = rows$last_tweet_id,
    last_tweet_at = if (is.na(rows$last_tweet_at)) {
      NULL
    } else {
      format(rows$last_tweet_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    }
  ))
}

#' Merge updates into the singleton twitter_state row (upsert).
#' @export
twitter_state_save <- function(...) {
  updates <- list(...)
  if (length(updates) == 0L) {
    return(invisible())
  }
  conn <- db_conn()
  # Build upsert
  cols <- names(updates)
  set_clauses <- paste(sprintf("%s = EXCLUDED.%s", cols, cols), collapse = ", ")
  col_list <- paste(c("id", cols), collapse = ", ")
  placeholders <- paste(c("1", sprintf("$%d", seq_along(cols))), collapse = ", ")
  sql <- sprintf(
    "INSERT INTO twitter_state (%s) VALUES (%s)
     ON CONFLICT (id) DO UPDATE SET %s, updated_at = now()",
    col_list,
    placeholders,
    set_clauses
  )
  dbExecute(conn, sql, params = unname(updates))
  return(invisible())
}

# ----------------------------------------------------------------------------
# alpaca_assets (full-table refresh)
# ----------------------------------------------------------------------------

#' Last refresh timestamp; Inf if empty.
#' @export
alpaca_assets_age_days <- function() {
  conn <- db_conn()
  r <- dbGetQuery(conn, "SELECT MAX(fetched_at) AS m FROM alpaca_assets")
  if (nrow(r) == 0L || is.na(r$m)) {
    return(Inf)
  }
  return(as.numeric(difftime(Sys.time(), r$m, units = "days")))
}

#' Read all tradable assets currently cached.
#' @export
alpaca_assets_read <- function() {
  conn <- db_conn()
  return(as.data.table(dbGetQuery(
    conn,
    "SELECT symbol, name, exchange, asset_class, tradable, fractionable, status FROM alpaca_assets"
  )))
}

#' Wipe + reinsert (full refresh). Accepts a data.table or data.frame with
#' at least the `symbol` column.
#' @export
alpaca_assets_replace <- function(assets) {
  conn <- db_conn()
  dt <- as.data.table(assets)
  keep <- c("symbol", "name", "exchange", "class", "tradable", "fractionable", "status")
  have <- intersect(keep, names(dt))
  dt <- dt[, ..have]
  if ("class" %in% names(dt)) {
    setnames(dt, "class", "asset_class")
  }
  dbBegin(conn)
  tryCatch(
    {
      dbExecute(conn, "TRUNCATE alpaca_assets")
      dbWriteTable(conn, "alpaca_assets", dt, append = TRUE, row.names = FALSE)
      dbCommit(conn)
    },
    error = function(e) {
      dbRollback(conn)
      stop(e)
    }
  )
  return(invisible())
}

# ----------------------------------------------------------------------------
# scraped_pages (URL cache with TTL)
# ----------------------------------------------------------------------------

#' Look up a cached scrape. Returns NULL if missing or stale beyond ttl_days.
#' @export
scraped_pages_get <- function(url, ttl_days = 7) {
  conn <- db_conn()
  r <- dbGetQuery(
    conn,
    "SELECT url, content, grade_usable, grade_reason, scraped_at
     FROM scraped_pages WHERE url = $1",
    params = list(url)
  )
  if (nrow(r) == 0L) {
    return(NULL)
  }
  age <- as.numeric(difftime(Sys.time(), r$scraped_at, units = "days"))
  if (age > ttl_days) {
    return(NULL)
  }
  return(list(
    content = r$content,
    grade_usable = isTRUE(r$grade_usable),
    grade_reason = r$grade_reason,
    age_days = age
  ))
}

.domain_of <- function(url) {
  m <- regmatches(url, regexec("^https?://([^/]+)", url))[[1]]
  return(if (length(m) >= 2L) tolower(m[2]) else "")
}

#' Upsert a scrape result.
#' @export
scraped_pages_put <- function(url, content, grade_usable, grade_reason) {
  conn <- db_conn()
  dbExecute(
    conn,
    "INSERT INTO scraped_pages (url, domain, content, grade_usable, grade_reason, scraped_at)
     VALUES ($1, $2, $3, $4, $5, now())
     ON CONFLICT (url) DO UPDATE SET
       domain = EXCLUDED.domain,
       content = EXCLUDED.content,
       grade_usable = EXCLUDED.grade_usable,
       grade_reason = EXCLUDED.grade_reason,
       scraped_at = EXCLUDED.scraped_at",
    params = list(url, .domain_of(url), content, grade_usable, grade_reason)
  )
  return(invisible())
}

# ----------------------------------------------------------------------------
# cycle_runs
# ----------------------------------------------------------------------------

#' Begin a cycle run. Returns the cycle_id.
#' @export
cycle_runs_start <- function(iteration) {
  conn <- db_conn()
  cid <- new_cycle_id()
  dbExecute(
    conn,
    "INSERT INTO cycle_runs (cycle_id, iteration) VALUES ($1, $2)",
    params = list(cid, as.integer(iteration))
  )
  return(cid)
}

#' Mark a cycle run finished.
#' @export
cycle_runs_finish <- function(cycle_id, success = TRUE, error_message = NA, picks = NA, decisions_summary = NA) {
  conn <- db_conn()
  dbExecute(
    conn,
    "UPDATE cycle_runs SET finished_at = now(), success = $2,
     error_message = $3, picks = $4, decisions_summary = $5
     WHERE cycle_id = $1",
    params = list(cycle_id, success, error_message, picks, decisions_summary)
  )
  return(invisible())
}

# ----------------------------------------------------------------------------
# tweets_sent
# ----------------------------------------------------------------------------

#' Record a sent tweet.
#' @export
tweets_sent_insert <- function(tweet_id, text, in_reply_to = NA, reason = NA) {
  conn <- db_conn()
  dbExecute(
    conn,
    "INSERT INTO tweets_sent (tweet_id, text, in_reply_to, reason)
     VALUES ($1, $2, $3, $4)
     ON CONFLICT (tweet_id) DO NOTHING",
    params = list(tweet_id, text, in_reply_to, reason)
  )
  return(invisible())
}
