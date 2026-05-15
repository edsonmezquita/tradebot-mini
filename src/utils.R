box::use(later)

#' @export
setInterval <- function(fun, interval = 60) {
  handle <- later$later(
    function() {
      fun()
      setInterval(fun, interval)
    },
    interval
  )
  return(invisible(handle))
}
