box::use(
  ./src/deepseek[ ask_with_tools ],
  ./src/tools[ TOOLS, TOOL_HANDLERS, tools_state ],
  ./src/prompts,
  ./src/features[ compute_features ]
)

cat("=== sanity: pure R compute_features with parameterised features ===\n")
box::use(./src/alpaca[market], lubridate)
fmt <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
now <- lubridate$now(tzone = "UTC")
bars <- market$get_bars_multi(
  symbols = c("AAPL", "NVDA"),
  timeframe = "1Day",
  start = fmt(now - lubridate$ddays(120)),
  end = fmt(now),
  feed = "iex"
)
feats <- compute_features(
  bars,
  features = list(
    list(name = "rsi", period = 7L),
    list(name = "rsi", period = 14L),
    list(name = "rsi", period = 21L),
    list(name = "macd", fast = 5L, slow = 13L, signal = 5L),
    list(name = "ema",  periods = c(9L, 21L, 50L))
  )
)
print(feats)

cat("\n=== full agent: model picks parameters itself ===\n")
res <- ask_with_tools(
  prompt = paste(
    "Today is 2026-05-15. AAPL and NVDA are mid-large-cap, decent volatility,",
    "I'm running a 1-2 week swing horizon.",
    "Step 1: fetch ~60 trading days of daily bars for AAPL and NVDA via get_bars_multi —",
    "        you decide the exact window.",
    "Step 2: compute_features. CHOOSE your own indicator parameters (RSI period(s),",
    "        MACD speeds, BB sigma, EMA cross periods, etc.) and JUSTIFY them in the final reply.",
    "Step 3: one-paragraph quant verdict per ticker."
  ),
  system = prompts$IDENTITY_TRADER,
  tools = TOOLS,
  handlers = TOOL_HANDLERS,
  max_tokens = 4000,
  max_iter = 8,
  verbose = TRUE
)
cat("\n--- final ---\n", res$content, "\n", sep = "")
