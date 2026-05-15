box::use(
  later,
  ./src/utils[ setInterval ],
  ./src/searxng[search]
)

setInterval(
  function() {
    news <- search(
      query = "Trump",
      engines = "google,yandex,baidu",
      pageno = 1
    )

    print(news)
  },
  10
)

while (!later$loop_empty()) {
  later$run_now()
}
