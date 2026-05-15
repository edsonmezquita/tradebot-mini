box::use(
  later,
  ./src/utils[ setInterval ]
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
