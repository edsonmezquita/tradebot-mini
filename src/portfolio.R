box::use(
  data.table[data.table, setorder],
  lubridate,
  ./alpaca[account, trading]
)

# ----------------------------------------------------------------------------
# Snapshots of account / positions / open orders, formatted as compact tables
# (or named lists) suitable for inserting into model prompts.
# ----------------------------------------------------------------------------

#' Cash, equity, buying power — compact named list, numerics coerced.
#' @export
get_account_state <- function() {
  acc <- account$get_account()
  return(list(
    cash = as.numeric(acc$cash),
    equity = as.numeric(acc$equity),
    portfolio_value = as.numeric(acc$portfolio_value),
    buying_power = as.numeric(acc$buying_power),
    long_market_val = as.numeric(acc$long_market_value),
    short_market_val = as.numeric(acc$short_market_value),
    daytrade_count = as.integer(acc$daytrade_count),
    pattern_day_trader = isTRUE(acc$pattern_day_trader)
  ))
}

#' Open positions joined with "days held" derived from the most recent BUY
#' fill per symbol (Alpaca doesn't store entry timestamp on positions).
#' @export
get_positions_with_age <- function() {
  pos <- account$get_positions()
  if (nrow(pos) == 0L) {
    return(data.table())
  }

  # Latest BUY fill per symbol → days_held.
  fills <- account$get_activities(activity_types = "FILL", page_size = 500L)
  buys <- fills[side == "buy"]
  setorder(buys, symbol, -transaction_time)
  last_buy <- buys[, list(last_buy_at = transaction_time[1]), by = symbol]

  out <- merge(
    pos[, list(
      symbol,
      qty = as.numeric(qty),
      avg_entry_price = as.numeric(avg_entry_price),
      current_price = as.numeric(current_price),
      market_value = as.numeric(market_value),
      unrealized_pl = as.numeric(unrealized_pl),
      unrealized_plpc = as.numeric(unrealized_plpc) * 100,
      side
    )],
    last_buy,
    by = "symbol",
    all.x = TRUE
  )

  now <- lubridate$now(tzone = "UTC")
  out[,
    days_held := as.numeric(
      difftime(now, lubridate$ymd_hms(last_buy_at, tz = "UTC"), units = "days")
    )
  ]
  out[, last_buy_at := NULL]
  return(out)
}

#' Open (unfilled) orders — compact view for the model.
#' @export
get_open_orders <- function() {
  ord <- trading$get_orders(status = "open")
  if (nrow(ord) == 0L) {
    return(data.table())
  }
  return(ord[, list(
    symbol,
    side,
    qty = as.numeric(qty),
    type,
    limit_price = as.numeric(limit_price),
    time_in_force,
    submitted_at
  )])
}
