box::use(later, lubridate)

#' @export
setInterval <- function(fun, interval = 60) {
  handle <- later$later(
    function() {
      setInterval(fun, interval)
      fun()
    },
    interval
  )
  return(invisible(handle))
}

#' @export
get_time_window <- function(window_size_d = lubridate$ddays(30)) {
  now <- lubridate$now(tzone = "UTC")
  then <- now - window_size_d
  fmt <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  return(list(
    then = fmt(then),
    now = fmt(now)
  ))
}
