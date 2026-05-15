box::use(
  later,
  ./src/utils[setInterval, get_time_window],
  ./src/searxng[search],
  ./src/alpaca[market]
)

iteration <- 0

setInterval(
  function() {
    time_window <- get_time_window()
    sprintf(
      "Running iteration %i\n\tCURRENT WINDOW: %s to %s",
      iteration,
      time_window$then,
      time_window$now
    )

    bars <- market$get_bars(
      symbol = "AAPL",
      timeframe = "1Day",
      start = time_window$then,
      end = time_window$now,
      feed = "iex"
    )
    news <- search(
      query = "Trump",
      engines = "google,yandex,baidu",
      pageno = 1
    )

    # print(news)
    print(bars)

    iteration <<- iteration + 1
  },
  3
)

while (!later$loop_empty()) {
  later$run_now()
}
