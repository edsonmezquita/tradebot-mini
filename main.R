box::use(
  lubridate,
  later,
  ./src/utils[setInterval, get_time_window],
  ./src/searxng[search],
  ./src/alpaca[market],
  ./src/deepseek[ ask, ask_with_tools ],
  ./src/prompts,
  ./src/tools[ TOOL_SEARCH, TOOL_HANDLERS ]
)

iteration <- 0

setInterval(
  function() {
    time_window <- get_time_window()
    cat(
      sprintf(
        "Running iteration %i\n\tCURRENT WINDOW: %s to %s",
        iteration,
        time_window$then,
        time_window$now
      ),
      "\n"
    )

    today <- format(lubridate$now(), "%Y-%m-%d")
    initial_query <- ask_with_tools(
      prompt = sprintf(
        "Today is %s. Find current market-moving news and pick a few tickers worth watching today. Limit yourself to about 3 searches, then summarise.",
        today
      ),
      system = prompts$IDENTITY_TRADER,
      tools = TOOL_SEARCH,
      handlers = TOOL_HANDLERS,
      max_tokens = 4000,
      max_iter = 15,
      verbose = TRUE
    )

    # d_news <- ask_with_tools(news)

    # bars <- market$get_bars(
    #   symbol = d_news$stocks,
    #   timeframe = "1Day",
    #   start = time_window$then,
    #   end = time_window$now,
    #   feed = "iex"
    # )

    # d_stocks <- ask_with_tools(news + bars)

    # market$buy(d_stocks$buy)
    # market$sell(d_stocks$sell)

    iteration <<- iteration + 1
  },
  10
)

while (!later$loop_empty()) {
  later$run_now()
}
