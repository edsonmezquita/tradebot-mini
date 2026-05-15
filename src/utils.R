box::use(later, lubridate)

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

#' @export
get_time_window <- function(window_size_d = lubridate$ddays(30)) {
  now <- lubridate$now()
  then <- now - window_size_d
  return(list(
    then = then,
    now = now
  ))
}
