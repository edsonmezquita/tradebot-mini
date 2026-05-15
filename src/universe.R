box::use(
  data.table[ as.data.table, fread, fwrite, setorder ],
  ./alpaca[ market ]
)

# In-memory cache (per R session).
.universe_cache <- new.env(parent = emptyenv())

# On-disk cache (persists across sessions). Refreshed when file age exceeds TTL.
.CACHE_PATH <- "cache/alpaca_universe.csv"
.CACHE_TTL_DAYS <- 30L

.cache_age_days <- function(path) {
  if (!file.exists(path)) {
    return(Inf)
  }
  as.numeric(difftime(Sys.time(), file.info(path)$mtime, units = "days"))
}

.fetch_universe_from_alpaca <- function() {
  assets <- market$get_assets(status = "active", asset_class = "us_equity")
  assets <- assets[tradable == TRUE]
  if ("attributes" %in% names(assets)) {
    assets[, attributes := NULL]
  }
  setorder(assets, symbol)
  return(assets)
}

#' Fetch (and cache) the universe of tradable US-equity assets on Alpaca.
#'
#' Caching:
#'   1. In-memory cache (per R session) — avoids re-reads.
#'   2. On-disk CSV at cache/alpaca_universe.csv — refreshed when older than
#'      `.CACHE_TTL_DAYS` (30 days) or when `refresh = TRUE` is passed.
#'
#' @param refresh Force a re-fetch from Alpaca even if cached.
#' @return data.table with cols symbol, name, exchange, tradable, fractionable, ...
#' @export
get_tradable_universe <- function(refresh = FALSE) {
  if (!refresh && exists("assets", envir = .universe_cache)) {
    return(get("assets", envir = .universe_cache))
  }

  age <- .cache_age_days(.CACHE_PATH)
  if (!refresh && age <= .CACHE_TTL_DAYS) {
    cat(sprintf("[universe] loading from disk cache (%.1f days old)\n", age))
    assets <- fread(.CACHE_PATH)
  } else {
    if (refresh) {
      cat("[universe] refresh=TRUE; fetching from Alpaca\n")
    } else {
      cat(sprintf("[universe] cache stale (%.1f days, ttl=%d); fetching from Alpaca\n", age, .CACHE_TTL_DAYS))
    }
    assets <- .fetch_universe_from_alpaca()
    if (!dir.exists(dirname(.CACHE_PATH))) {
      dir.create(dirname(.CACHE_PATH), recursive = TRUE)
    }
    fwrite(assets, .CACHE_PATH)
    cat(sprintf("[universe] wrote %d assets to %s\n", nrow(assets), .CACHE_PATH))
  }

  assign("assets", assets, envir = .universe_cache)
  return(assets)
}

#' Validate a vector of tickers against the Alpaca-tradable universe.
#'
#' @param picks Character vector of ticker symbols (case-insensitive).
#' @return list(valid = <chr>, invalid = <chr>) — both upper-cased.
#' @export
validate_picks <- function(picks) {
  picks <- toupper(as.character(picks))
  universe <- get_tradable_universe()
  ok <- picks %in% universe$symbol
  list(
    valid = picks[ok],
    invalid = picks[!ok]
  )
}
