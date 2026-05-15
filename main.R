box::use(
  later,
  jsonlite[ fromJSON ],
  data.table[ tail ],
  utils[ capture.output ],
  ./src/utils[ setInterval, get_time_window ],
  ./src/deepseek[ ask, ask_with_tools ],
  ./src/prompts,
  ./src/tools[ TOOL_SEARCH, TOOL_GET_BARS, TOOL_COMPUTE_FEATURES, TOOL_HANDLERS, tools_state ],
  ./src/features[ format_feature_menu ],
  ./src/signals[ derive_signals ]
)

iteration <- 0

run_pipeline <- function() {
  time_window <- get_time_window()
  cat(sprintf(
    "\n========== iteration %i  %s ==========\n",
    iteration,
    time_window$now
  ))

  cat("\n[stage 1] research\n")
  research_response <- ask_with_tools(
    prompt = sprintf(
      paste(
        "Today is %s.",
        "Use the search tool (1-3 queries — you choose how many) to find current",
        "market-moving news. Then reply ONLY with JSON of the form:",
        '{ "picks": ["TICKER1","TICKER2",...], "rationale": "one paragraph: why these tickers, what news drives them" }',
        "Pick 2-4 US-listed tickers. No prose outside the JSON."
      ),
      time_window$now
    ),
    system = prompts$IDENTITY_TRADER,
    tools = list(TOOL_SEARCH),
    handlers = TOOL_HANDLERS,
    json = TRUE,
    max_tokens = 4000,
    max_iter = 6,
    verbose = TRUE
  )
  research <- fromJSON(research_response$content)
  cat("  picks:", paste(research$picks, collapse = ", "), "\n")

  cat("\n[stage 2] bars\n")
  bars_response <- ask_with_tools(
    prompt = sprintf(
      paste(
        "Picks from research: %s.",
        "Rationale: %s",
        "",
        "Call get_bars_multi to fetch the right history for these tickers.",
        "YOU decide:",
        "  - timeframe (1Day, 1Hour, 15Min, ...)",
        "  - days of history",
        "based on swing-trade horizon (1d-1mo) and the asset's character.",
        "",
        "After the tool returns, reply ONLY with JSON:",
        '{ "bars_id": "...", "timeframe": "...", "days": N, "justification": "why these choices for these names" }'
      ),
      paste(research$picks, collapse = ", "),
      research$rationale
    ),
    system = prompts$IDENTITY_TRADER,
    tools = list(TOOL_GET_BARS),
    handlers = TOOL_HANDLERS,
    json = TRUE,
    max_tokens = 4000,
    max_iter = 4,
    verbose = TRUE
  )
  bars_meta <- fromJSON(bars_response$content)
  cat(sprintf(
    "  bars_id=%s  timeframe=%s  days=%s\n",
    bars_meta$bars_id,
    bars_meta$timeframe,
    bars_meta$days
  ))

  cat("\n[stage 3] features (model picks specs via tool call; R does the math)\n")
  features_response <- ask_with_tools(
    prompt = sprintf(
      paste(
        "Tickers: %s. Rationale: %s",
        "",
        "Bars are already cached at bars_id='%s' (timeframe=%s, days=%s).",
        "",
        "Call compute_features ONCE on that bars_id with a tailored set of indicators",
        "for these names and this swing-trade horizon.",
        "Multi-instance same indicator with different params is encouraged",
        "(e.g. rsi period=7 AND period=21 for divergence).",
        "",
        "After the tool returns, reply ONLY with JSON:",
        '{ "justification": "why you chose these indicators with these parameters" }'
      ),
      paste(research$picks, collapse = ", "),
      research$rationale,
      bars_meta$bars_id,
      bars_meta$timeframe,
      bars_meta$days
    ),
    system = prompts$IDENTITY_TRADER,
    tools = list(TOOL_COMPUTE_FEATURES),
    handlers = TOOL_HANDLERS,
    json = TRUE,
    max_tokens = 4000,
    max_iter = 4,
    verbose = TRUE
  )
  features_analysis <- fromJSON(features_response$content)

  bars <- tools_state$bars[[bars_meta$bars_id]]
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
      "\n\nLast 10 bars per symbol with the indicators YOU chose to compute",
      "(parameter choices encoded in the column names):\n",
      recent_text,
      "\n\nYour earlier justification for the indicator parameters:\n",
      features_analysis$justification,
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
}

setInterval(run_pipeline, 60 * 60 * 3)

while (!later$loop_empty()) {
  later$run_now()
}
