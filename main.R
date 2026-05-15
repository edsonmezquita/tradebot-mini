box::use(
  lubridate,
  later,
  jsonlite[ fromJSON ],
  ./src/utils[ setInterval, get_time_window ],
  ./src/alpaca[ market ],
  ./src/deepseek[ ask, ask_with_tools ],
  ./src/prompts,
  ./src/tools[ TOOL_SEARCH, TOOL_HANDLERS ],
  ./src/features[ compute_features, FEATURE_REGISTRY ]
)

iteration <- 0

setInterval(function() {
  time_window <- get_time_window()
  cat(sprintf(
    "Iteration %i  window: %s to %s\n",
    iteration, time_window$then, time_window$now
  ))

  cat("\n[stage 1] research\n")
  research <- ask_with_tools(
    prompt = sprintf(paste(
      "Today is %s.",
      "Use search (1-3 queries) to find current market moving news.",
      "Then reply ONLY with a JSON object of the form:",
      '{ "picks": ["TICKER1", "TICKER2", ...], "rationale": "one-paragraph summary of the news driving each pick" }',
      "Pick 2-4 US-listed tickers. No prose outside the JSON."
    ), time_window$now),
    system = prompts$IDENTITY_TRADER,
    tools = list(TOOL_SEARCH),
    handlers = TOOL_HANDLERS,
    max_tokens = 4000,
    max_iter = 6,
    verbose = TRUE
  )
  picks <- fromJSON(research$content)
  cat("  picks:", paste(picks$picks, collapse = ", "), "\n")

  cat("\n[stage 2] fetch bars from Alpaca\n")
  bars <- market$get_bars_multi(
    symbols   = picks$picks,
    timeframe = "1Day",
    start     = time_window$then,
    end       = time_window$now,
    feed      = "iex"
  )
  cat("  got", nrow(bars), "bars across", length(picks$picks), "tickers\n")

  cat("\n[stage 3] compute features\n")
  feats <- compute_features(
    bars     = bars,
    features = c("rsi14", "macd", "bbands", "atr14", "supertrend", "ema_cross", "composite")
  )
  print(feats)

  cat("\n[stage 4] decide\n")
  decision_raw <- ask(
    prompt = paste(
      "News rationale from earlier research:\n", picks$rationale, "\n\n",
      "Computed indicators per ticker:\n",
      paste(capture.output(print(feats)), collapse = "\n"), "\n\n",
      'Reply ONLY with a JSON object of the form:',
      '{ "decisions": [ { "ticker": "X", "action": "buy"|"sell"|"hold", "confidence": 0.0-1.0, "reason": "..." }, ... ] }'
    ),
    system = prompts$IDENTITY_TRADER,
    json = TRUE,
    max_tokens = 4000
  )
  decisions <- fromJSON(decision_raw, simplifyDataFrame = TRUE)
  print(decisions$decisions)

  # ---- STAGE 5: execute (stubbed for now) --------------------------------
  # buys  <- decisions$decisions[decisions$decisions$action == "buy",  "ticker"]
  # sells <- decisions$decisions[decisions$decisions$action == "sell", "ticker"]
  # if (length(buys))  market$buy(buys)
  # if (length(sells)) market$sell(sells)

  iteration <<- iteration + 1
}, 60 * 60 * 3)

while (!later$loop_empty()) {
  later$run_now()
}
