box::use(
  later,
  jsonlite[ fromJSON ],
  ./src/utils[ setInterval, get_time_window ],
  ./src/deepseek[ ask, ask_with_tools ],
  ./src/prompts,
  ./src/tools[
    TOOL_SEARCH,
    TOOL_GET_BARS,
    TOOL_COMPUTE_FEATURES,
    TOOL_HANDLERS
  ]
)

iteration <- 0

run_pipeline <- function() {
  tw <- get_time_window()
  cat(sprintf(
    "\n========== iteration %i  %s ==========\n",
    iteration, tw$now
  ))

  cat("\n[stage 1] research — model picks tickers from news\n")
  s1 <- ask_with_tools(
    prompt = sprintf(paste(
      "Today is %s.",
      "Use the search tool (1-3 queries — you choose how many) to find current",
      "market-moving news. Then reply ONLY with JSON of the form:",
      '{ "picks": ["TICKER1","TICKER2",...], "rationale": "one paragraph: why these tickers, what is the news driving them" }',
      "Pick 2-4 US-listed tickers. No prose outside the JSON."
    ), tw$now),
    system    = prompts$IDENTITY_TRADER,
    tools     = list(TOOL_SEARCH),
    handlers  = TOOL_HANDLERS,
    json      = TRUE,
    max_tokens = 4000,
    max_iter  = 6,
    verbose   = TRUE
  )
  research <- fromJSON(s1$content)
  cat("  picks:", paste(research$picks, collapse = ", "), "\n")

  cat("\n[stage 2] bars — model chooses timeframe and history window\n")
  s2 <- ask_with_tools(
    prompt = sprintf(paste(
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
    ), paste(research$picks, collapse = ", "), research$rationale),
    system    = prompts$IDENTITY_TRADER,
    tools     = list(TOOL_GET_BARS),
    handlers  = TOOL_HANDLERS,
    json      = TRUE,
    max_tokens = 4000,
    max_iter  = 4,
    verbose   = TRUE
  )
  bars_meta <- fromJSON(s2$content)
  cat(sprintf(
    "  bars_id=%s  timeframe=%s  days=%s\n",
    bars_meta$bars_id, bars_meta$timeframe, bars_meta$days
  ))

  cat("\n[stage 3] features — model chooses indicators and parameters\n")
  s3 <- ask_with_tools(
    prompt = sprintf(paste(
      "Tickers: %s.",
      "Rationale: %s",
      "Bars already fetched: bars_id='%s', timeframe=%s, days=%s.",
      "",
      "Call compute_features (you may call it multiple times if useful) on this bars_id.",
      "CHOOSE each indicator AND its parameters deliberately based on the asset's character.",
      "Multi-instance same indicator with different params is encouraged",
      "(e.g. rsi period=7 AND period=21 for divergence).",
      "",
      "After your tool calls, reply ONLY with JSON:",
      '{ "indicators_table": "<paste the table you got back as a markdown table or readable text>",',
      '  "specs_used": [ { "name": "...", ...params } ],',
      '  "justification": "why you chose these indicators with these parameters" }'
    ), paste(research$picks, collapse = ", "), research$rationale,
       bars_meta$bars_id, bars_meta$timeframe, bars_meta$days),
    system    = prompts$IDENTITY_TRADER,
    tools     = list(TOOL_COMPUTE_FEATURES),
    handlers  = TOOL_HANDLERS,
    json      = TRUE,
    max_tokens = 6000,
    max_iter  = 6,
    verbose   = TRUE
  )
  analysis <- fromJSON(s3$content)
  cat("  indicators chosen:\n")
  print(analysis$specs_used)

  cat("\n[stage 4] decide\n")
  s4 <- ask(
    prompt = paste(
      "News rationale:\n", research$rationale, "\n\n",
      "Indicator analysis (with the parameters you chose):\n",
      analysis$indicators_table, "\n\n",
      "Your justification for those parameters:\n", analysis$justification, "\n\n",
      "Reply ONLY with JSON of the form:",
      '{ "decisions": [ { "ticker": "X", "action": "buy"|"sell"|"hold", "confidence": 0.0-1.0, "reason": "one sentence grounded in the indicators above" } ] }'
    ),
    system     = prompts$IDENTITY_TRADER,
    json       = TRUE,
    max_tokens = 4000
  )
  decisions <- fromJSON(s4$content, simplifyDataFrame = TRUE)
  cat("\n--- decisions ---\n")
  print(decisions$decisions)

  # ---- STAGE 5: execute (stubbed for now) --------------------------------
  # buys  <- decisions$decisions[decisions$decisions$action == "buy",  "ticker"]
  # sells <- decisions$decisions[decisions$decisions$action == "sell", "ticker"]
  # if (length(buys))  market$buy(buys)
  # if (length(sells)) market$sell(sells)

  iteration <<- iteration + 1
  invisible(decisions)
}

setInterval(run_pipeline, 60 * 60 * 3)

while (!later$loop_empty()) {
  later$run_now()
}
