box::use(
  ./searxng[ search ]
)

#' OpenAI-format tool schemas exposed to the model.
#' @export
TOOL_SEARCH <- list(list(
  type = "function",
  `function` = list(
    name = "search",
    description = paste(
      "Search the web via a self-hosted SearXNG instance.",
      "Returns up to 10 results (title, url, content snippet, engine).",
      "Use to gather news, prices, sentiment, or any external info."
    ),
    parameters = list(
      type = "object",
      properties = list(
        query = list(
          type = "string",
          description = "Search query, e.g. 'AAPL earnings May 2026'"
        ),
        engines = list(
          type = "string",
          description = paste(
            "Optional comma-separated engines to restrict to.",
            "Available: google, bing, duckduckgo, brave, mojeek, qwant,",
            "startpage, yandex, baidu. Omit to use all enabled engines."
          )
        ),
        language = list(
          type = "string",
          description = "Language code, e.g. 'en' or 'all'. Default 'all'."
        )
      ),
      required = list("query")
    )
  )
))

#' Handlers for the schemas above. Names must match `function$name`.
#' Each handler takes one named-list arg (parsed JSON the model sent).
#' @export
TOOL_HANDLERS <- list(
  search = function(args) {
    language <- args$language
    if (is.null(language) || !nzchar(language)) language <- "all"
    res <- search(
      query = args$query,
      engines = args$engines,
      language = language,
      pageno = 1
    )
    if (nrow(res) > 10) res <- res[seq_len(10)]
    return(res)
  }
)
