box::use(
  data.table[ data.table, fread, fwrite, setorder ],
  lubridate
)

.MEMOS_PATH <- "state/memos.csv"

#' Append a cycle memo to state/memos.csv. Creates file/dir if missing.
#' @param picks character vector of tickers considered this cycle.
#' @param decisions list from TOOL_SUBMIT_TRADES (each: ticker, action, notional, ...).
#' @param memo one short factual paragraph from the model.
#' @export
write_memo <- function(picks, decisions, memo) {
  buys  <- character()
  sells <- character()
  holds <- character()
  for (d in decisions) {
    if (d$action == "buy") {
      buys <- c(buys, sprintf("%s:%g", d$ticker, d$notional))
    } else if (d$action == "sell") {
      sells <- c(sells, sprintf("%s:%g", d$ticker, d$notional))
    } else if (d$action == "hold") {
      holds <- c(holds, d$ticker)
    }
  }

  row <- data.table(
    timestamp = format(lubridate$now(tzone = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
    picks     = paste(picks,  collapse = ","),
    buys      = paste(buys,   collapse = ","),
    sells     = paste(sells,  collapse = ","),
    holds     = paste(holds,  collapse = ","),
    memo      = memo
  )

  if (!dir.exists(dirname(.MEMOS_PATH))) {
    dir.create(dirname(.MEMOS_PATH), recursive = TRUE)
  }
  fwrite(row, .MEMOS_PATH, append = file.exists(.MEMOS_PATH))
  invisible(row)
}

#' Read memos with optional filtering.
#' @param limit integer, max rows to return.
#' @param order "newest" or "oldest".
#' @param ticker optional, only memos that mention this symbol.
#' @param since optional ISO timestamp floor (string compare on the timestamp col).
#' @param until optional ISO timestamp ceiling.
#' @return data.table (possibly empty).
#' @export
read_memos <- function(
  limit  = 10L,
  order  = "newest",
  ticker = NULL,
  since  = NULL,
  until  = NULL
) {
  if (!file.exists(.MEMOS_PATH)) return(data.table())
  m <- fread(.MEMOS_PATH)
  if (nrow(m) == 0L) return(m)

  if (!is.null(ticker) && nzchar(ticker)) {
    pat <- paste0("(^|,)", toupper(ticker), "(:|,|$)")
    m <- m[
      grepl(pat, picks) | grepl(pat, buys) |
      grepl(pat, sells) | grepl(pat, holds)
    ]
  }
  if (!is.null(since) && nzchar(since)) m <- m[timestamp >= since]
  if (!is.null(until) && nzchar(until)) m <- m[timestamp <= until]

  setorder(m, timestamp)
  if (order == "newest") m <- m[order(-timestamp)]
  if (!is.null(limit) && nrow(m) > limit) m <- m[seq_len(limit)]
  return(m)
}
