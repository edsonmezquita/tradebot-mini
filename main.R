box::use(
  later,
  ./src/utils[setInterval, get_time_window],
  ./src/searxng[search],
  ./src/alpaca[market],
  ./src/deepseek[ ask, ask_with_tools ]
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

    news <- search(
      query = "Trump",
      engines = "google,yandex,baidu",
      pageno = 1
    )

    d_news <- ask_with_tools(news)

    bars <- market$get_bars(
      symbol = d_news$stocks,
      timeframe = "1Day",
      start = time_window$then,
      end = time_window$now,
      feed = "iex"
    )

    d_stocks <- ask_with_tools(news + bars)

    market$buy(d_stocks$buy)
    market$sell(d_stocks$sell)

    iteration <<- iteration + 1
  },
  60 * 60 * 3
)

while (!later$loop_empty()) {
  later$run_now()
}
