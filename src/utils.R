box::use(later, lubridate, data.table[as.data.table, copy])

#' @export
setInterval <- function(fun, interval = 60) {
  handle <- later$later(
    function() {
      setInterval(fun, interval)
      return(fun())
    },
    interval
  )
  return(invisible(handle))
}

#' Render a data.table as a Markdown pipe table — the format LLMs parse most
#' reliably (per 2025 benchmarks: better than CSV/TSV/pipe, fewer tokens than
#' JSON). Numeric columns are rounded to `digits`.
#' @export
format_dt_md <- function(dt, digits = 4L) {
  if (is.null(dt) || nrow(dt) == 0L) {
    return("(empty)")
  }
  dt <- copy(as.data.table(dt))
  for (col in names(dt)) {
    if (is.numeric(dt[[col]])) {
      dt[, (col) := round(get(col), digits)]
    }
  }
  hdr <- paste0("| ", paste(names(dt), collapse = " | "), " |")
  sep <- paste0("|", paste(rep("---", ncol(dt)), collapse = "|"), "|")
  rows <- vapply(
    seq_len(nrow(dt)),
    function(i) {
      return(paste0("| ", paste(as.character(unlist(dt[i])), collapse = " | "), " |"))
    },
    character(1)
  )
  return(paste(c(hdr, sep, rows), collapse = "\n"))
}

#' @export
get_time_window <- function(window_size_d = lubridate$ddays(30)) {
  now <- lubridate$now(tzone = "UTC")
  then <- now - window_size_d
  fmt <- function(t) {
    return(format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  }
  return(list(
    then = fmt(then),
    now = fmt(now)
  ))
}
