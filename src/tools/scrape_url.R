box::use(
  jsonlite[ fromJSON ],
  ../scrape[ scrape ],
  ../deepseek[ ask ]
)

#' @export
TOOL_SCRAPE_URL <- list(
  type = "function",
  `function` = list(
    name = "scrape_url",
    description = paste(
      "Fetch and clean a single web page (returns markdown body).",
      "Use when a search snippet looks promising but you need the full article",
      "(numbers, quotes, guidance, etc.) to make a confident decision.",
      "Slow (1-5s) and many news sites have paywalls — content is graded",
      "automatically and rejected scrapes will return a notice.",
      "Cap yourself at ~3 scrapes per cycle."
    ),
    parameters = list(
      type = "object",
      properties = list(
        url = list(
          type = "string",
          description = "Full URL to scrape, e.g. 'https://www.cnbc.com/2026/05/15/...'"
        )
      ),
      required = list("url")
    )
  )
)

# Grade scraped content with a cheap, separate DeepSeek call. Returns TRUE if
# the content is a usable news article body, FALSE if it's a paywall, login
# wall, error page, or other garbage.
.grade_content <- function(content) {
  judgement_raw <- ask(
    prompt = paste(
      "You are evaluating a web page scrape result. Decide whether the text",
      "below contains usable article/news content (paragraphs of substance,",
      "quotes, numbers, reporting) or whether it's effectively a paywall,",
      "login wall, cookie banner, navigation chrome, error page, or empty stub.",
      "",
      "Reply ONLY with JSON: { \"usable\": true|false, \"reason\": \"one short phrase\" }",
      "",
      "--- scraped content ---",
      content
    ),
    model      = "deepseek-v4-flash",
    json       = TRUE,
    max_tokens = 500
  )
  tryCatch(
    fromJSON(judgement_raw),
    error = function(e) list(usable = TRUE, reason = "grader-failed-defaulting-true")
  )
}

#' @export
handle_scrape_url <- function(args) {
  content <- scrape(args$url)
  if (startsWith(content, "(scrape failed")) {
    return(content)
  }
  grade <- .grade_content(content)
  if (isTRUE(grade$usable)) {
    return(content)
  }
  return(sprintf("(scrape rejected: %s)", as.character(grade$reason)))
}
