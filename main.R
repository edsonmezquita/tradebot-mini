box::use(
  later,
  lubridate,
  jsonlite[ fromJSON ],
  data.table[ rbindlist, tail ],
  utils[ capture.output ],
  ./src/utils[ setInterval, get_time_window, format_dt_md ],
  ./src/deepseek[ ask, ask_with_tools, ask_for_args ],
  ./src/prompts,
  ./src/tools[
    TOOL_SEARCH, TOOL_VALIDATE_SYMBOLS,
    TOOL_GET_BARS_MULTI, TOOL_COMPUTE_FEATURES,
    TOOL_SUBMIT_TRADES, TOOL_HANDLERS
  ],
  ./src/features[ compute_features ],
  ./src/signals[ derive_signals ],
  ./src/alpaca[ market, trading ],
  ./src/portfolio[ get_account_state, get_positions_with_age, get_open_orders ]
)

iteration <- 0

setInterval(
  function() {
    time_window <- get_time_window()
    cat(sprintf(
      "\n========== iteration %i  %s - %s ==========\n",
      iteration,
      time_window$then,
      time_window$now
    ))

    cat("\n[stage 1] research\n")
    research_response <- ask_with_tools(
      prompt = sprintf(
        paste(
          "Today is %s.",
          "1) Use the search tool (1-3 queries) to find current market-moving news.",
          "2) Draft 2-4 candidate tickers (US-listed, NYSE/NASDAQ/ARCA/AMEX, common stock — use exact exchange symbols, e.g. BRK.B not BERKSHIRE).",
          "3) Call validate_symbols on your draft. Replace any invalid ones and re-validate until all are valid.",
          "4) Reply ONLY with JSON of the form:",
          '{ "picks": ["TICKER1","TICKER2",...], "rationale": "one paragraph: why these tickers, what news drives them" }',
          "No prose outside the JSON."
        ),
        time_window$now
      ),
      system = prompts$IDENTITY_TRADER,
      tools = list(TOOL_SEARCH, TOOL_VALIDATE_SYMBOLS),
      handlers = TOOL_HANDLERS,
      json = TRUE,
      max_tokens = 4000,
      max_iter = 8,
      verbose = TRUE
    )
    research <- fromJSON(research_response$content)
    cat("  picks:", paste(research$picks, collapse = ", "), "\n")

    cat("\n[stage 2] bars\n")
    bars_args <- ask_for_args(
      prompt = sprintf(
        paste(
          "Picks: %s",
          "Rationale: %s",
          "",
          "Pick the timeframe and history window appropriate for a swing trade",
          "(1 day to 1 month) on these specific names. Then submit via the tool."
        ),
        paste(research$picks, collapse = ", "),
        research$rationale
      ),
      system = prompts$IDENTITY_TRADER,
      tool = TOOL_GET_BARS_MULTI,
      max_tokens = 2000
    )
    cat(sprintf(
      "  symbols=%s  timeframe=%s  days=%s\n  rationale: %s\n",
      paste(unlist(bars_args$symbols), collapse = ","),
      bars_args$timeframe,
      bars_args$days,
      bars_args$rationale
    ))

    now <- lubridate$now(tzone = "UTC")
    then <- now - lubridate$ddays(as.integer(bars_args$days))
    fmt <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    bars <- market$get_bars_multi(
      symbols = unlist(bars_args$symbols, use.names = FALSE),
      timeframe = bars_args$timeframe,
      start = fmt(then),
      end = fmt(now),
      feed = "iex"
    )
    cat("  fetched", nrow(bars), "bars\n")

    cat("\n[stage 3] features\n")
    feat_args <- ask_for_args(
      prompt = sprintf(
        paste(
          "Tickers: %s.  Bars: %s, %s days, %d rows.",
          "News rationale: %s",
          "",
          "Pick a tailored set of indicators (and parameters per indicator) that",
          "best characterise these names for a 1d-1mo swing horizon. Multi-instance",
          "same indicator with different params is encouraged."
        ),
        paste(research$picks, collapse = ", "),
        bars_args$timeframe,
        bars_args$days,
        nrow(bars),
        research$rationale
      ),
      system = prompts$IDENTITY_TRADER,
      tool = TOOL_COMPUTE_FEATURES,
      max_tokens = 3000
    )
    cat("  specs chosen:\n")
    str(feat_args$features)
    cat("  rationale:", feat_args$rationale, "\n")

    bars <- compute_features(bars, feat_args$features)
    cat("  added cols:", paste(attr(bars, "feature_cols"), collapse = ", "), "\n")

    cat("\n[stage 3.5] R-derived signal (for our eyes only — not fed to model)\n")
    signals <- derive_signals(bars)
    print(signals)

    cat("\n[stage 4] decide\n")
    acct        <- get_account_state()
    positions   <- get_positions_with_age()
    open_orders <- get_open_orders()
    recent      <- bars[, tail(.SD, 10L), by = symbol]

    cat(sprintf("  cash=$%.0f  equity=$%.0f  buying_power=$%.0f  positions=%d  open_orders=%d\n",
                acct$cash, acct$equity, acct$buying_power, nrow(positions), nrow(open_orders)))

    decision_args <- ask_for_args(
      prompt = paste(
        "<account_state>\n",
        sprintf("cash=%.2f  equity=%.2f  buying_power=%.2f  daytrade_count=%d  pattern_day_trader=%s",
                acct$cash, acct$equity, acct$buying_power,
                acct$daytrade_count, acct$pattern_day_trader),
        "\n</account_state>\n\n",

        "<positions> (current holdings; respect minimum hold periods)\n",
        format_dt_md(positions, digits = 4),
        "\n</positions>\n\n",

        "<open_orders> (unfilled — do NOT duplicate)\n",
        format_dt_md(open_orders, digits = 4),
        "\n</open_orders>\n\n",

        "<news_rationale>\n", research$rationale, "\n</news_rationale>\n\n",

        "<recent_bars> (last 10 bars per symbol with the indicators you chose;",
        " parameter choices encoded in the column names)\n",
        format_dt_md(recent, digits = 4),
        "\n</recent_bars>\n\n",

        "<indicator_rationale>\n", feat_args$rationale, "\n</indicator_rationale>\n\n",

        "Submit one decision per ticker via the submit_trades tool.",
        "Action 'hold' = no order. For 'buy'/'sell' include a notional dollar amount",
        "you'd risk on that single trade, sized appropriately given buying_power",
        "and the confidence you have in the setup."
      ),
      system     = prompts$IDENTITY_TRADER,
      tool       = TOOL_SUBMIT_TRADES,
      max_tokens = 6000
    )
    decisions <- decision_args$decisions
    cat("\n--- model decisions ---\n")
    for (d in decisions) {
      cat(sprintf("  %s  %-4s  $%-8s  conf=%.2f  %s\n",
                  d$ticker, d$action,
                  if (is.null(d$notional)) "" else format(d$notional, nsmall = 2),
                  d$confidence, d$reason))
    }

    cat("\n--- comparison: programmatic vs model ---\n")
    decisions_dt <- rbindlist(lapply(decisions, function(d) {
      list(symbol = d$ticker, model_action = d$action, model_conf = d$confidence)
    }))
    print(merge(
      signals[, list(symbol, prog_signal = signal, prog_score = score)],
      decisions_dt,
      by = "symbol", all = TRUE
    ))

    cat("\n[stage 5] execute\n")
    for (d in decisions) {
      if (d$action == "hold") {
        cat(sprintf("  HOLD  %s\n", d$ticker))
        next
      }
      result <- tryCatch(
        trading$add_order(
          symbol        = d$ticker,
          side          = d$action,
          type          = "market",
          time_in_force = "day",
          notional      = d$notional
        ),
        error = function(e) {
          cat(sprintf("  FAIL  %s %s $%s: %s\n",
                      d$ticker, d$action, d$notional, conditionMessage(e)))
          NULL
        }
      )
      if (!is.null(result)) {
        cat(sprintf("  OK    %s %s $%s  (order_id=%s)\n",
                    d$ticker, d$action, d$notional,
                    if (is.list(result)) result$id else "?"))
      }
    }

    iteration <<- iteration + 1
    invisible(list(research = research, bars = bars, signals = signals, decisions = decisions))
  },
  60 * 60 * 3
)

while (!later$loop_empty()) {
  later$run_now()
}
