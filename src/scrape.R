box::use(
  httr2,
  jsonlite[toJSON]
)

# Rough char-to-token ratio. Truncate to ~3000 tokens ≈ 12000 chars.
.MAX_CHARS <- 12000L

#' Scrape a single URL via the local crawl4ai container and return cleaned
#' markdown. Returns a character scalar. Truncated to ~3000 tokens.
#' @param url URL to scrape.
#' @param instance Base URL of the crawl4ai service. Defaults to env CRAWL4AI_URL or localhost:11235.
#' @param timeout Seconds. Default 60 (page rendering can be slow).
#' @export
scrape <- function(
  url,
  instance = Sys.getenv("CRAWL4AI_URL", "http://localhost:11235"),
  timeout = 60
) {
  stopifnot(nzchar(url))

  body <- list(url = url, f = "fit")

  resp <- httr2$request(instance) |>
    httr2$req_url_path("/md") |>
    httr2$req_headers(`Content-Type` = "application/json") |>
    httr2$req_body_raw(toJSON(body, auto_unbox = TRUE), type = "application/json") |>
    httr2$req_timeout(timeout) |>
    httr2$req_user_agent("tradebot-mini/0.1") |>
    httr2$req_error(is_error = function(resp) FALSE) |>
    httr2$req_perform()

  status <- httr2$resp_status(resp)
  if (status >= 400) {
    return(sprintf("(scrape failed: HTTP %d)", status))
  }

  parsed <- httr2$resp_body_json(resp, simplifyVector = FALSE)
  md <- parsed$markdown
  if (is.null(md) || !nzchar(md)) {
    return("(scrape failed: empty response)")
  }

  if (nchar(md) > .MAX_CHARS) {
    md <- paste0(substr(md, 1L, .MAX_CHARS), "\n\n[...truncated...]")
  }
  return(md)
}
