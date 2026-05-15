box::use(
  later,
  ./src/utils[ setInterval ],
  ./src/searxng[search]
)

setInterval(
  function() {
    print("Hello world!")
  },
  3
)

while (!later$loop_empty()) {
  later$run_now()
}
