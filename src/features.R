box::use(
  hpfi,
  data.table[ as.data.table, setorder, copy, fifelse ],
  stats[ lm, coef ],
  utils[ tail ]
)

# ---- helpers ---------------------------------------------------------------
.tag <- function(x) {
  if (is.integer(x) || x == as.integer(x)) {
    return(as.character(as.integer(x)))
  }
  return(gsub("\\.", "p", as.character(x)))
}
.tag_join <- function(...) paste(vapply(list(...), .tag, character(1)), collapse = "_")

.arg <- function(spec, name, default) {
  v <- spec[[name]]
  if (is.null(v)) default else v
}

# Rolling slope of a numeric vector over a window (least-squares fit on
# index 1..window). Returns NA for the first (window-1) positions.
.rolling_slope <- function(x, window) {
  n <- length(x)
  out <- rep(NA_real_, n)
  if (n < window) {
    return(out)
  }
  xs <- seq_len(window)
  for (i in window:n) {
    y <- x[(i - window + 1L):i]
    if (!any(is.na(y))) {
      out[i] <- unname(coef(lm(y ~ xs))[2])
    }
  }
  out
}

# ---- one function per indicator -------------------------------------------
# Contract: each function MUTATES bars by adding columns via `:=`, grouped by
# symbol. Returns the character vector of column names it added.

feature_rsi <- function(bars, params) {
  period <- as.integer(.arg(params, "period", 14L))
  col <- paste0("rsi_", .tag(period))
  bars[, (col) := hpfi$ind_mom_rsi(close, period), by = symbol]
  return(col)
}

feature_macd <- function(bars, params) {
  fast <- as.integer(.arg(params, "fast", 12L))
  slow <- as.integer(.arg(params, "slow", 26L))
  signal <- as.integer(.arg(params, "signal", 9L))
  tag <- .tag_join(fast, slow, signal)
  cols <- c(
    paste0("macd_", tag),
    paste0("macd_signal_", tag),
    paste0("macd_hist_", tag)
  )
  bars[,
    (cols) := {
      m <- hpfi$ind_mom_macd(close, fast, slow, signal)
      list(m$macd, m$signal, m$histogram)
    },
    by = symbol
  ]
  return(cols)
}

feature_bbands <- function(bars, params) {
  period <- as.integer(.arg(params, "period", 20L))
  sd <- as.numeric(.arg(params, "sd", 2.0))
  tag <- .tag_join(period, sd)
  cols <- c(paste0("bb_pct_b_", tag), paste0("bb_bw_", tag))
  bars[,
    (cols) := {
      bb <- hpfi$ind_vol_bbands(close, period, sd)
      list(bb$bb_pct_b, bb$bb_bw)
    },
    by = symbol
  ]
  return(cols)
}

feature_atr <- function(bars, params) {
  period <- as.integer(.arg(params, "period", 14L))
  col <- paste0("atr_", .tag(period))
  bars[, (col) := hpfi$ind_vol_atr(high, low, close, period), by = symbol]
  return(col)
}

feature_supertrend <- function(bars, params) {
  period <- as.integer(.arg(params, "period", 10L))
  mult <- as.numeric(.arg(params, "multiplier", 3.0))
  col <- paste0("st_dir_", .tag_join(period, mult))
  bars[,
    (col) := {
      st <- hpfi$ind_trend_supertrend(high, low, close, period, mult)
      st$direction
    },
    by = symbol
  ]
  return(col)
}

feature_ema <- function(bars, params) {
  periods <- as.integer(unlist(.arg(params, "periods", c(20L, 50L))))
  added <- character()
  for (p in periods) {
    col <- paste0("ema_", .tag(p))
    bars[, (col) := hpfi$ewm_ema(close, p), by = symbol]
    added <- c(added, col)
  }
  if (length(periods) >= 2L) {
    sorted <- sort(periods)
    for (i in seq_len(length(sorted) - 1L)) {
      short_p <- sorted[i]
      long_p <- sorted[i + 1L]
      cross_col <- paste0("ema_cross_", .tag(short_p), "_", .tag(long_p))
      short_col <- paste0("ema_", .tag(short_p))
      long_col <- paste0("ema_", .tag(long_p))
      bars[,
        (cross_col) := fifelse(
          is.na(get(short_col)) | is.na(get(long_col)),
          NA_integer_,
          fifelse(get(short_col) > get(long_col), 1L, -1L)
        )
      ]
      added <- c(added, cross_col)
    }
  }
  return(added)
}

feature_obv_slope <- function(bars, params) {
  window <- as.integer(.arg(params, "window", 10L))
  col <- paste0("obv_slope_", .tag(window))
  bars[, (col) := .rolling_slope(hpfi$ind_volume_obv(close, volume), window), by = symbol]
  return(col)
}

feature_composite <- function(bars, params) {
  col <- "composite"
  bars[,
    (col) := {
      rsi <- hpfi$ind_mom_rsi(close, 14L)
      bb <- hpfi$ind_vol_bbands(close, 20L, 2.0)
      m <- hpfi$ind_mom_macd(close)
      atr <- hpfi$ind_vol_atr(high, low, close, 14L)
      s_rsi <- hpfi$score_rsi(rsi)
      s_bb <- hpfi$score_bb(bb$bb_pct_b)
      s_macd <- hpfi$score_macd(m$histogram, atr)
      rowMeans(cbind(s_rsi, s_bb, s_macd), na.rm = TRUE)
    },
    by = symbol
  ]
  return(col)
}

# ---- registry: name -> {description, defaults, fn} ------------------------
#' @export
FEATURE_REGISTRY <- list(
  rsi = list(
    description = "RSI momentum oscillator. >70 overbought, <30 oversold.",
    params = list(period = 14L),
    fn = feature_rsi
  ),
  macd = list(
    description = "MACD trend/momentum. Adds macd, signal, hist columns.",
    params = list(fast = 12L, slow = 26L, signal = 9L),
    fn = feature_macd
  ),
  bbands = list(
    description = "Bollinger Bands. Adds bb_pct_b (0-1 within band) and bb_bw (band width).",
    params = list(period = 20L, sd = 2.0),
    fn = feature_bbands
  ),
  atr = list(
    description = "Average True Range volatility. Useful for stop sizing.",
    params = list(period = 14L),
    fn = feature_atr
  ),
  supertrend = list(
    description = "SuperTrend regime indicator. dir column: 1 = uptrend, -1 = downtrend.",
    params = list(period = 10L, multiplier = 3.0),
    fn = feature_supertrend
  ),
  ema = list(
    description = "EMAs at one or more periods. Multiple periods also yield cross signals between consecutive lengths.",
    params = list(periods = c(20L, 50L)),
    fn = feature_ema
  ),
  obv_slope = list(
    description = "Rolling slope of On-Balance Volume. Positive = accumulation.",
    params = list(window = 10L),
    fn = feature_obv_slope
  ),
  composite = list(
    description = "Composite [-1,1] score blending RSI/BB/MACD scores at standard parameters.",
    params = list(),
    fn = feature_composite
  )
)

# ---- composer + helpers ----------------------------------------------------

#' Render FEATURE_REGISTRY as a model/human-readable menu string.
#' @export
format_feature_menu <- function() {
  paste(
    vapply(
      names(FEATURE_REGISTRY),
      function(n) {
        e <- FEATURE_REGISTRY[[n]]
        p <- if (length(e$params) == 0L) {
          "(no params)"
        } else {
          paste(
            vapply(
              names(e$params),
              function(k) sprintf("%s=%s", k, paste(e$params[[k]], collapse = ",")),
              character(1)
            ),
            collapse = ", "
          )
        }
        sprintf("- %s [defaults: %s]: %s", n, p, e$description)
      },
      character(1)
    ),
    collapse = "\n"
  )
}

#' Compute features and ADD them as columns to bars (via `:=`).
#' @param bars data.table with cols symbol, timestamp, open, high, low, close, volume.
#' @param features list of feature spec objects, e.g.
#'   list(
#'     list(name = "rsi", period = 14L),
#'     list(name = "rsi", period = 21L),
#'     list(name = "macd", fast = 12L, slow = 26L, signal = 9L),
#'     list(name = "ema",  periods = c(9L, 21L, 50L))
#'   )
#' @return The mutated bars data.table (a copy of the input — caller's bars is
#'   not modified). The set of added column names is on attr(., "feature_cols").
#' @export
compute_features <- function(bars, features) {
  if (!is.list(features) || length(features) == 0L) {
    stop("`features` must be a non-empty list of feature spec objects.")
  }
  bars <- copy(as.data.table(bars))
  setorder(bars, symbol, timestamp)

  added <- character()
  for (spec in features) {
    entry <- FEATURE_REGISTRY[[spec$name]]
    if (is.null(entry)) {
      warning("Unknown feature: '", spec$name, "' — skipped.")
      next
    }
    added <- c(added, entry$fn(bars, spec))
  }
  attr(bars, "feature_cols") <- unique(added)
  return(bars)
}

#' Take the latest row per symbol from a bars-with-features table.
#' Used by the agentic tool path to summarise back to the model.
#' @export
latest_per_symbol <- function(bars_with_features) {
  bf <- as.data.table(bars_with_features)
  setorder(bf, symbol, timestamp)
  bf[, tail(.SD, 1L), by = symbol]
}
