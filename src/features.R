box::use(
  hpfi,
  data.table[ as.data.table, setorder ],
  stats[ lm, coef ]
)

#' Names + descriptions of every feature the model can request.
#' Keep this list short and curated — every entry shows up in the tool schema.
#' @export
FEATURE_REGISTRY <- list(
  rsi14 = "RSI(14) momentum oscillator. >70 overbought, <30 oversold.",
  macd = "MACD(12,26,9). Returns macd, signal, hist. Bullish when hist > 0 and rising.",
  bbands = "Bollinger Bands(20, 2sigma). Returns pct_b (0-1 within band) and bw (band width).",
  atr14 = "Average True Range(14) — volatility, useful for stop sizing.",
  supertrend = "SuperTrend(10, 3) trend regime. Returns dir: 1 = uptrend, -1 = downtrend.",
  ema_cross = "EMA(20) vs EMA(50). Returns ema20, ema50, cross: 1 if 20>50 (bullish), -1 otherwise.",
  obv_slope = "Slope of On-Balance Volume over the last 10 bars. Positive = accumulation.",
  composite = "Composite [-1,1] score blending RSI / BB / MACD score functions."
)

.last <- function(x) x[length(x)]

.compute_one <- function(close, high, low, volume, features) {
  res <- list(
    last_close = .last(close),
    n_bars = length(close)
  )

  if ("rsi14" %in% features) {
    res$rsi14 <- .last(hpfi$ind_mom_rsi(close, 14L))
  }
  if ("macd" %in% features) {
    m <- hpfi$ind_mom_macd(close)
    res$macd <- .last(m$macd)
    res$macd_signal <- .last(m$signal)
    res$macd_hist <- .last(m$histogram)
  }
  if ("bbands" %in% features) {
    bb <- hpfi$ind_vol_bbands(close, 20L, 2.0)
    res$bb_pct_b <- .last(bb$bb_pct_b)
    res$bb_bw <- .last(bb$bb_bw)
  }
  if ("atr14" %in% features) {
    res$atr14 <- .last(hpfi$ind_vol_atr(high, low, close, 14L))
  }
  if ("supertrend" %in% features) {
    st <- hpfi$ind_trend_supertrend(high, low, close, 10L, 3.0)
    res$st_dir <- .last(st$direction)
  }
  if ("ema_cross" %in% features) {
    e20 <- .last(hpfi$ewm_ema(close, 20L))
    e50 <- .last(hpfi$ewm_ema(close, 50L))
    res$ema20 <- e20
    res$ema50 <- e50
    res$ema_cross <- if (is.na(e20) || is.na(e50)) {
      NA_integer_
    } else if (e20 > e50) {
      1L
    } else {
      -1L
    }
  }
  if ("obv_slope" %in% features) {
    obv <- hpfi$ind_volume_obv(close, volume)
    n <- length(obv)
    if (n >= 10 && !any(is.na(tail(obv, 10)))) {
      x <- seq_len(10)
      y <- obv[(n - 9):n]
      res$obv_slope10 <- unname(coef(lm(y ~ x))[2])
    } else {
      res$obv_slope10 <- NA_real_
    }
  }
  if ("composite" %in% features) {
    rsi <- hpfi$ind_mom_rsi(close, 14L)
    bb <- hpfi$ind_vol_bbands(close, 20L, 2.0)
    m <- hpfi$ind_mom_macd(close)
    atr <- hpfi$ind_vol_atr(high, low, close, 14L)
    s_rsi <- hpfi$score_rsi(rsi)
    s_bb <- hpfi$score_bb(bb$bb_pct_b)
    s_macd <- hpfi$score_macd(m$histogram, atr)
    res$composite <- mean(
      c(.last(s_rsi), .last(s_bb), .last(s_macd)),
      na.rm = TRUE
    )
  }
  return(res)
}

#' Compute selected features for each symbol from a bars data.table.
#' Pure transform — does NOT fetch data. Pass bars from market$get_bars_multi().
#' @param bars data.table with columns: symbol, timestamp, open, high, low, close, volume.
#' @param features character vector — subset of names(FEATURE_REGISTRY).
#' @return data.table with one row per symbol, columns = symbol, last_close, n_bars,
#'         and the requested feature values.
#' @export
compute_features <- function(bars, features) {
  features <- intersect(features, names(FEATURE_REGISTRY))
  if (length(features) == 0L) {
    stop(
      "No valid features requested. Available: ",
      paste(names(FEATURE_REGISTRY), collapse = ", ")
    )
  }

  bars <- as.data.table(bars)
  setorder(bars, symbol, timestamp)

  out <- bars[,
    .compute_one(close, high, low, volume, features),
    by = symbol
  ]
  return(out)
}
