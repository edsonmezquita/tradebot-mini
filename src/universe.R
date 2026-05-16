box::use(
  data.table[as.data.table, setorder],
  ./alpaca[market],
  ./db[alpaca_assets_age_days, alpaca_assets_read, alpaca_assets_replace]
)

.CACHE_TTL_DAYS <- 30L

# Per-session in-memory cache so repeated calls within one cycle skip the DB.
.mem_cache <- new.env(parent = emptyenv())

.fetch_universe_from_alpaca <- function() {
  assets <- market$get_assets(status = "active", asset_class = "us_equity")
  assets <- assets[tradable == TRUE]
  if ("attributes" %in% names(assets)) {
    assets[, attributes := NULL]
  }
  setorder(assets, symbol)
  return(assets)
}

#' Fetch (and cache) the tradable US-equity universe. Cached in the DB —
#' refreshed once `.CACHE_TTL_DAYS` (30) days old, or when `refresh = TRUE`.
#' @export
get_tradable_universe <- function(refresh = FALSE) {
  if (!refresh && exists("assets", envir = .mem_cache)) {
    return(get("assets", envir = .mem_cache))
  }

  age <- alpaca_assets_age_days()
  if (!refresh && age <= .CACHE_TTL_DAYS) {
    cat(sprintf("[universe] loading from DB cache (%.1f days old)\n", age))
    assets <- alpaca_assets_read()
  } else {
    if (refresh) {
      cat("[universe] refresh=TRUE; fetching from Alpaca\n")
    } else {
      cat(sprintf("[universe] cache stale (%.1f days, ttl=%d); fetching from Alpaca\n", age, .CACHE_TTL_DAYS))
    }
    assets <- .fetch_universe_from_alpaca()
    alpaca_assets_replace(assets)
    cat(sprintf("[universe] wrote %d assets to DB\n", nrow(assets)))
  }

  assign("assets", assets, envir = .mem_cache)
  return(assets)
}

#' Validate a vector of tickers against the universe.
#' @return list(valid = <chr>, invalid = <chr>)
#' @export
validate_picks <- function(picks) {
  picks <- toupper(as.character(picks))
  universe <- get_tradable_universe()
  ok <- picks %in% universe$symbol
  return(list(
    valid = picks[ok],
    invalid = picks[!ok]
  ))
}
