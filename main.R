box::use(
  later,
  ./src/utils[ setInterval ],
  ./src/searxng[search]
)

setInterval(
  function() {
    news <- search(
      query = "Trump",
      categories = "news,general",
      engines = "google,yandex,baidu",
      pageno = 1
    )

    print(news)
  },
  60 * 60 * 24 * 7
)

while (!later$loop_empty()) {
  later$run_now()
}
