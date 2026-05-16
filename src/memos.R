box::use(
  ./db[memos_insert, memos_read, new_cycle_id]
)

#' Append a cycle memo to the database.
#' @param picks character vector of tickers considered this cycle.
#' @param decisions list from TOOL_SUBMIT_TRADES (each: ticker, action, notional, ...).
#' @param memo one short factual paragraph from the model.
#' @param cycle_id Optional pre-generated cycle id (UUID string). If NULL one is created.
#' @export
write_memo <- function(picks, decisions, memo, cycle_id = NULL) {
  buys <- character()
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
  if (is.null(cycle_id)) {
    cycle_id <- new_cycle_id()
  }
  memos_insert(
    cycle_id = cycle_id,
    picks = paste(picks, collapse = ","),
    buys = paste(buys, collapse = ","),
    sells = paste(sells, collapse = ","),
    holds = paste(holds, collapse = ","),
    memo = memo
  )
  return(invisible())
}

#' Read memos with optional filtering. Thin pass-through to the DB layer.
#' @export
read_memos <- function(limit = 10L, order = "newest", ticker = NULL, since = NULL, until = NULL) {
  return(memos_read(limit = limit, order = order, ticker = ticker, since = since, until = until))
}
