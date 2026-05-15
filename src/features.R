box::use(
  hpfi,
  data.table[ as.data.table, setorder ],
  stats[ lm, coef ],
  utils[ tail ]
)

# ---- helpers ---------------------------------------------------------------
.last <- function(x) x[length(x)]

# Stringify a numeric/integer for a column-name tag: 14 -> "14", 2.0 -> "2", 0.5 -> "0p5"
.tag <- function(x) {
  if (is.integer(x) || x == as.integer(x)) {
    return(as.character(as.integer(x)))
  }
  return(gsub("\\.", "p", as.character(x)))
}
.tag_join <- function(...) paste(vapply(list(...), .tag, character(1)), collapse = "_")

# Pull a parameter from a spec, or fall back to a default
.arg <- function(spec, name, default) {
  v <- spec[[name]]
  if (is.null(v)) default else v
}

# ---- one function per indicator -------------------------------------------
# Contract: each function takes a per-symbol bars data.table and a params list,
# and returns a NAMED list of result columns. The keys become column names in
# the final output, so embed the params in them (e.g. "rsi_14").

feature_rsi <- function(bars, params) {
  period <- as.integer(.arg(params, "period", 14L))
  setNames(
    list(.last(hpfi$ind_mom_rsi(bars$close, period))),
    paste0("rsi_", .tag(period))
  )
}

feature_macd <- function(bars, params) {
  fast   <- as.integer(.arg(params, "fast",   12L))
  slow   <- as.integer(.arg(params, "slow",   26L))
  signal <- as.integer(.arg(params, "signal", 9L))
  m <- hpfi$ind_mom_macd(bars$close, fast, slow, signal)
  tag <- .tag_join(fast, slow, signal)
  setNames(
    list(.last(m$macd), .last(m$signal), .last(m$histogram)),
    c(paste0("macd_", tag), paste0("macd_signal_", tag), paste0("macd_hist_", tag))
  )
}

feature_bbands <- function(bars, params) {
  period <- as.integer(.arg(params, "period", 20L))
  sd     <- as.numeric(.arg(params, "sd",     2.0))
  bb <- hpfi$ind_vol_bbands(bars$close, period, sd)
  tag <- .tag_join(period, sd)
  setNames(
    list(.last(bb$bb_pct_b), .last(bb$bb_bw)),
    c(paste0("bb_pct_b_", tag), paste0("bb_bw_", tag))
  )
}

feature_atr <- function(bars, params) {
  period <- as.integer(.arg(params, "period", 14L))
  setNames(
    list(.last(hpfi$ind_vol_atr(bars$high, bars$low, bars$close, period))),
    paste0("atr_", .tag(period))
  )
}

feature_supertrend <- function(bars, params) {
  period <- as.integer(.arg(params, "period",     10L))
  mult   <- as.numeric(.arg(params, "multiplier", 3.0))
  st <- hpfi$ind_trend_supertrend(bars$high, bars$low, bars$close, period, mult)
  setNames(
    list(.last(st$direction)),
    paste0("st_dir_", .tag_join(period, mult))
  )
}

feature_ema <- function(bars, params) {
  periods <- as.integer(unlist(.arg(params, "periods", c(20L, 50L))))
  out <- list()
  for (p in periods) {
    out[[paste0("ema_", .tag(p))]] <- .last(hpfi$ewm_ema(bars$close, p))
  }
  # Add cross signals between consecutive periods (sorted short -> long)
  if (length(periods) >= 2L) {
    sorted <- sort(periods)
    for (i in seq_len(length(sorted) - 1L)) {
      short_p <- sorted[i]
      long_p  <- sorted[i + 1L]
      es <- .last(hpfi$ewm_ema(bars$close, short_p))
      el <- .last(hpfi$ewm_ema(bars$close, long_p))
      out[[paste0("ema_cross_", .tag(short_p), "_", .tag(long_p))]] <- if (is.na(es) || is.na(el)) {
        NA_integer_
      } else if (es > el) {
        1L
      } else {
        -1L
      }
    }
  }
  return(out)
}

feature_obv_slope <- function(bars, params) {
  window <- as.integer(.arg(params, "window", 10L))
  obv <- hpfi$ind_volume_obv(bars$close, bars$volume)
  n <- length(obv)
  key <- paste0("obv_slope_", .tag(window))
  v <- if (n >= window && !any(is.na(tail(obv, window)))) {
    x <- seq_len(window)
    y <- obv[(n - window + 1L):n]
    unname(coef(lm(y ~ x))[2])
  } else {
    NA_real_
  }
  setNames(list(v), key)
}

feature_composite <- function(bars, params) {
  # Standard-parameter composite — intentionally not parameterised.
  rsi <- hpfi$ind_mom_rsi(bars$close, 14L)
  bb  <- hpfi$ind_vol_bbands(bars$close, 20L, 2.0)
  m   <- hpfi$ind_mom_macd(bars$close)
  atr <- hpfi$ind_vol_atr(bars$high, bars$low, bars$close, 14L)
  list(composite = mean(
    c(
      .last(hpfi$score_rsi(rsi)),
      .last(hpfi$score_bb(bb$bb_pct_b)),
      .last(hpfi$score_macd(m$histogram, atr))
    ),
    na.rm = TRUE
  ))
}

# ---- registry: name -> {description, defaults, fn} ------------------------
# Single source of truth used by both the tool schema and the dispatcher.
# Add a new indicator: write feature_xyz(), append a registry entry.
#' @export
FEATURE_REGISTRY <- list(
  rsi = list(
    description = "RSI momentum oscillator. >70 overbought, <30 oversold.",
    params      = list(period = 14L),
    fn          = feature_rsi
  ),
  macd = list(
    description = "MACD trend/momentum. Returns macd, signal, hist columns.",
    params      = list(fast = 12L, slow = 26L, signal = 9L),
    fn          = feature_macd
  ),
  bbands = list(
    description = "Bollinger Bands. Returns bb_pct_b (0-1 within band) and bb_bw (band width).",
    params      = list(period = 20L, sd = 2.0),
    fn          = feature_bbands
  ),
  atr = list(
    description = "Average True Range volatility. Useful for stop sizing.",
    params      = list(period = 14L),
    fn          = feature_atr
  ),
  supertrend = list(
    description = "SuperTrend regime indicator. dir: 1 = uptrend, -1 = downtrend.",
    params      = list(period = 10L, multiplier = 3.0),
    fn          = feature_supertrend
  ),
  ema = list(
    description = "EMAs at one or more periods. Multiple periods also yield cross signals between consecutive lengths.",
    params      = list(periods = c(20L, 50L)),
    fn          = feature_ema
  ),
  obv_slope = list(
    description = "Slope of On-Balance Volume over a recent window. Positive = accumulation.",
    params      = list(window = 10L),
    fn          = feature_obv_slope
  ),
  composite = list(
    description = "Composite [-1,1] score blending RSI/BB/MACD scores at standard parameters.",
    params      = list(),
    fn          = feature_composite
  )
)

# ---- composer --------------------------------------------------------------
.compute_one <- function(bars, feature_specs) {
  res <- list(last_close = .last(bars$close), n_bars = nrow(bars))
  for (spec in feature_specs) {
    entry <- FEATURE_REGISTRY[[spec$name]]
    if (is.null(entry)) {
      warning("Unknown feature: '", spec$name, "' — skipped.")
      next
    }
    res <- c(res, entry$fn(bars, spec))
  }
  return(res)
}

#' Compute selected features (each with its own parameters) per symbol.
#' @param bars data.table from market$get_bars_multi (cols: symbol, timestamp,
#'   open, high, low, close, volume).
#' @param features list of feature spec objects, e.g.
#'   list(
#'     list(name = "rsi", period = 14L),
#'     list(name = "rsi", period = 21L),
#'     list(name = "macd", fast = 12L, slow = 26L, signal = 9L),
#'     list(name = "ema",  periods = c(9L, 21L, 50L))
#'   )
#' @return data.table, one row per symbol, columns named by indicator+params.
#' @export
compute_features <- function(bars, features) {
  if (!is.list(features) || length(features) == 0L) {
    stop("`features` must be a non-empty list of feature spec objects.")
  }
  bars <- as.data.table(bars)
  setorder(bars, symbol, timestamp)
  out <- bars[, .compute_one(.SD, features), by = symbol]
  return(out)
}
