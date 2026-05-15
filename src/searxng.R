box::use(
  httr2,
  data.table[ data.table, rbindlist ]
)

#' Query a SearXNG instance and return results as a data.table.
#'
#' @param query Search string.
#' @param categories Comma-separated string, e.g. "general", "news",
#'   or "general,news". NULL = instance default.
#' @param engines Comma-separated engines to restrict to (e.g. "google,yandex").
#' @param language Language code ("en", "all", ...).
#' @param pageno Result page number.
#' @param instance Base URL of the SearXNG instance.
#' @param timeout Request timeout in seconds.
#' @export
search <- function(
  query,
  categories = NULL,
  engines = NULL,
  language = "all",
  pageno = 1,
  instance = Sys.getenv("SEARXNG_URL"),
  timeout = 15
) {
  stopifnot(nzchar(query))

  params <- list(
    q = query,
    format = "json",
    language = language,
    pageno = pageno,
    categories = categories,
    engines = engines
  )
  params <- params[!vapply(params, is.null, logical(1))]

  resp <- httr2$request(instance) |>
    httr2$req_url_path("search") |>
    httr2$req_url_query(!!!params) |>
    httr2$req_timeout(timeout) |>
    httr2$req_user_agent("tradebot-mini/0.1") |>
    httr2$req_perform()

  body <- httr2$resp_body_json(resp, simplifyVector = FALSE)

  if (length(body$results) == 0) {
    return(data.table(
      title = character(),
      url = character(),
      content = character(),
      engine = character()
    ))
  }

  return(rbindlist(
    lapply(body$results, function(r) {
      list(
        title = if (is.null(r$title)) NA_character_ else r$title,
        url = if (is.null(r$url)) NA_character_ else r$url,
        content = if (is.null(r$content)) NA_character_ else r$content,
        engine = if (is.null(r$engine)) NA_character_ else r$engine
      )
    }),
    fill = TRUE
  ))
}
