box::use(
  later,
  ./src/utils[setInterval, get_time_window],
  ./src/searxng[search],
  ./src/alpaca[market]
)


time_window <- get_time_window()

bars <- market$get_bars(
  symbol = "AAPL",
  timeframe = "1Day",
  start = time_window$then,
  end = time_window$now
)

setInterval(
  function() {
    print(get_time_window())
    # news <- search(
    #   query = "Trump",
    #   engines = "google,yandex,baidu",
    #   pageno = 1
    # )

    # print(news)
  },
  10
)

while (!later$loop_empty()) {
  later$run_now()
}
