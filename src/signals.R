box::use(
  data.table[as.data.table, setorder, rbindlist],
  utils[tail]
)

# Each rule fn takes a row (named list) + a state list (score, reasons) and
# may mutate state. They scan the row for columns matching a name pattern, so
# whatever indicators the model picked, the matching rules fire.

.add <- function(state, delta, msg) {
  state$score <- state$score + as.integer(delta)
  state$reasons <- c(state$reasons, sprintf("%s (%+d)", msg, as.integer(delta)))
  return(state)
}

.rule_rsi <- function(row, state) {
  for (col in grep("^rsi_\\d+$", names(row), value = TRUE)) {
    v <- row[[col]]
    if (is.na(v)) {
      next
    }
    if (v < 30) {
      state <- .add(state, 2L, sprintf("%s=%.1f oversold", col, v))
    } else if (v > 70) {
      state <- .add(state, -2L, sprintf("%s=%.1f overbought", col, v))
    } else if (v < 50) {
      state <- .add(state, 1L, sprintf("%s=%.1f below midline", col, v))
    } else {
      state <- .add(state, -1L, sprintf("%s=%.1f above midline", col, v))
    }
  }
  return(state)
}

.rule_macd_hist <- function(row, state) {
  for (col in grep("^macd_hist_", names(row), value = TRUE)) {
    v <- row[[col]]
    if (is.na(v)) {
      next
    }
    if (v > 0) {
      state <- .add(state, 2L, sprintf("%s=%.3f bullish", col, v))
    } else {
      state <- .add(state, -2L, sprintf("%s=%.3f bearish", col, v))
    }
  }
  return(state)
}

.rule_supertrend <- function(row, state) {
  for (col in grep("^st_dir_", names(row), value = TRUE)) {
    v <- row[[col]]
    if (is.na(v)) {
      next
    }
    if (v == 1L) {
      state <- .add(state, 2L, sprintf("%s=uptrend", col))
    } else {
      state <- .add(state, -2L, sprintf("%s=downtrend", col))
    }
  }
  return(state)
}

.rule_ema_cross <- function(row, state) {
  for (col in grep("^ema_cross_", names(row), value = TRUE)) {
    v <- row[[col]]
    if (is.na(v)) {
      next
    }
    if (v == 1L) {
      state <- .add(state, 1L, sprintf("%s=bullish", col))
    } else {
      state <- .add(state, -1L, sprintf("%s=bearish", col))
    }
  }
  return(state)
}

.rule_bbands <- function(row, state) {
  for (col in grep("^bb_pct_b_", names(row), value = TRUE)) {
    v <- row[[col]]
    if (is.na(v)) {
      next
    }
    if (v > 1.0) {
      state <- .add(state, -1L, sprintf("%s=%.2f above upper band", col, v))
    } else if (v < 0.0) {
      state <- .add(state, 1L, sprintf("%s=%.2f below lower band", col, v))
    }
  }
  return(state)
}

.rule_obv_slope <- function(row, state) {
  for (col in grep("^obv_slope_", names(row), value = TRUE)) {
    v <- row[[col]]
    if (is.na(v)) {
      next
    }
    if (v > 0) {
      state <- .add(state, 1L, sprintf("%s>0 accumulation", col))
    } else {
      state <- .add(state, -1L, sprintf("%s<0 distribution", col))
    }
  }
  return(state)
}

.rule_composite <- function(row, state) {
  if (!"composite" %in% names(row)) {
    return(state)
  }
  v <- row$composite
  if (is.na(v)) {
    return(state)
  }
  if (v > 0.3) {
    state <- .add(state, 1L, sprintf("composite=%+.2f bullish", v))
  } else if (v < -0.3) {
    state <- .add(state, -1L, sprintf("composite=%+.2f bearish", v))
  }
  return(state)
}

RULES <- list(
  .rule_rsi,
  .rule_macd_hist,
  .rule_supertrend,
  .rule_ema_cross,
  .rule_bbands,
  .rule_obv_slope,
  .rule_composite
)

#' Derive a deterministic BUY/SELL/HOLD per symbol from the latest row of
#' a bars-with-indicators table. Score thresholds: >= +3 BUY, <= -3 SELL.
#'
#' This is a programmatic, transparent rule layer. It examines whatever
#' indicator columns are present (matched by name pattern), so it works with
#' any subset/parameterisation the model chose.
#' @export
derive_signals <- function(bars_with_features) {
  bf <- as.data.table(bars_with_features)
  setorder(bf, symbol, timestamp)
  latest <- bf[, tail(.SD, 1L), by = symbol]

  out <- vector("list", nrow(latest))
  for (i in seq_len(nrow(latest))) {
    row <- as.list(latest[i])
    state <- list(score = 0L, reasons = character())
    for (rule in RULES) {
      state <- rule(row, state)
    }
    signal <- if (state$score >= 3L) {
      "BUY"
    } else if (state$score <= -3L) {
      "SELL"
    } else {
      "HOLD"
    }
    out[[i]] <- list(
      symbol = row$symbol,
      signal = signal,
      score = state$score,
      reasons = paste(state$reasons, collapse = "; ")
    )
  }
  return(rbindlist(out))
}
