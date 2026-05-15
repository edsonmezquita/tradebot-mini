#' @export
TOOL_SEARCH <- list(list(
  type = "function",
  `function` = list(
    name = "search",
    description = "Query a SearXNG instance and return results as a data.table.",
    parameters = list(
      type = "object",
      properties = list(
        query = list(
          type = "string",
          description = "Search query"
        ),
        engines = list(
          type = "string",
          description = 'Comma-separated string, e.g. "general", "news", or "general,news". NULL = instance default.',
        ),
        language = list(
          type = "string",
          description = 'Language code ("en", "all", ...).'
        )
      ),
      required = list("query", "engines", "language")
    )
  )
))