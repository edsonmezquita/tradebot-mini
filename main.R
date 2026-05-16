box::use(
  later,
  lubridate,
  jsonlite[ fromJSON ],
  data.table[ as.data.table, tail ],
  utils[ capture.output ],
  ./src/utils[ setInterval, get_time_window ],
  ./src/deepseek[ ask, ask_with_tools, ask_for_args, extract_tool_results ],
  ./src/prompts,
  ./src/tools[
    TOOL_SEARCH, TOOL_VALIDATE_SYMBOLS,
    TOOL_GET_BARS_MULTI, TOOL_COMPUTE_FEATURES,
    TOOL_HANDLERS
  ],
  ./src/features[ compute_features ],
  ./src/signals[ derive_signals ],
  ./src/alpaca[ market ],
  ./src/universe[ validate_picks ]
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
    max_retries <- 3
    research_response <- ask_with_tools(
      prompt = sprintf(
        paste(
          "Today is %s.",
          "1) Use the search tool (1-3 queries) to find current market-moving news.",
          "2) Draft 2-4 candidate tickers (US-listed, NYSE/NASDAQ/ARCA/AMEX, common stock — use exact exchange symbols, e.g. BRK.B not BERKSHIRE).",
          "3) Call validate_symbols on your draft. If any are invalid, replace them and validate again.",
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
    prior_news <- extract_tool_results(research_response$messages)

    for (attempt in seq_len(max_retries + 1L)) {
      validation <- validate_picks(research$picks)
      if (length(validation$invalid) > 0) {
        cat("  dropped invalid picks:", paste(validation$invalid, collapse = ", "), "\n")
      }
      if (length(validation$valid) > 0) {
        research$picks <- validation$valid
        cat("  picks:", paste(research$picks, collapse = ", "), "\n")
        break
      }
      if (attempt > max_retries) {
        stop("All picks invalid after ", max_retries, " retries; aborting iteration.")
      }
      cat(sprintf("  retry %d/%d: all picks invalid, re-asking with prior news\n", attempt, max_retries))
      retry_response <- ask(
        prompt = paste(
          "Earlier news searches returned the following results:\n\n",
          prior_news,
          "\n\nYour previous picks were ALL rejected — these are NOT tradable on Alpaca: ",
          paste(validation$invalid, collapse = ", "),
          "\n\nPick 2-4 DIFFERENT US-listed tickers from the news above that ARE tradable on",
          "Alpaca (NYSE / NASDAQ / ARCA / AMEX). Use exact exchange symbols (e.g. BRK.B not BERKSHIRE).",
          "\n\nReply ONLY with JSON: { \"picks\": [...], \"rationale\": \"one paragraph\" }"
        ),
        system = prompts$IDENTITY_TRADER,
        json = TRUE,
        max_tokens = 4000
      )
      research <- fromJSON(retry_response$content)
    }

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
    recent <- bars[, tail(.SD, 10L), by = symbol]
    recent_text <- paste(capture.output(print(recent)), collapse = "\n")

    decision_response <- ask(
      prompt = paste(
        "News rationale (from your earlier research):\n",
        research$rationale,
        "\n\nLast 10 bars per symbol with the indicators YOU chose",
        "(parameter choices encoded in the column names):\n",
        recent_text,
        "\n\nYour earlier rationale for the indicator parameters:\n",
        feat_args$rationale,
        "\n\nReply ONLY with JSON of the form:",
        '{ "decisions": [ { "ticker": "X", "action": "buy"|"sell"|"hold", "confidence": 0.0-1.0, "reason": "one sentence grounded in the table above" } ] }'
      ),
      system = prompts$IDENTITY_TRADER,
      json = TRUE,
      max_tokens = 4000
    )
    decisions <- fromJSON(decision_response$content, simplifyDataFrame = TRUE)
    cat("\n--- model decisions ---\n")
    print(decisions$decisions)

    cat("\n--- comparison: programmatic vs model ---\n")
    print(merge(
      signals[, list(symbol, prog_signal = signal, prog_score = score)],
      as.data.table(decisions$decisions)[, list(symbol = ticker, model_action = action, model_conf = confidence)],
      by = "symbol",
      all = TRUE
    ))

    iteration <<- iteration + 1
    invisible(list(research = research, bars = bars, signals = signals, decisions = decisions))
  },
  60 * 60 * 3
)

while (!later$loop_empty()) {
  later$run_now()
}
