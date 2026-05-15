box::use(
  ./searxng[ search ],
  ./alpaca[ market ],
  lubridate
)

TOOL_SEARCH <- list(
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
)

TOOL_GET_BARS <- list(
  type = "function",
  `function` = list(
    name = "get_bars_multi",
    description = paste(
      "Fetch historical OHLCV bars for one or more US stock tickers from Alpaca.",
      "Returns a table with symbol, timestamp, open, high, low, close, volume, vwap.",
      "Pass multiple symbols in one call rather than calling repeatedly — it's much faster.",
      "Use after identifying tickers of interest from news to inspect price action."
    ),
    parameters = list(
      type = "object",
      properties = list(
        symbols = list(
          type = "array",
          description = "Ticker symbols, e.g. ['AAPL', 'MSFT', 'NVDA']",
          items = list(type = "string")
        ),
        days = list(
          type = "integer",
          description = "How many days of history to pull, ending today. Default 30."
        ),
        timeframe = list(
          type = "string",
          description = "Bar size, e.g. '1Day', '1Hour', '15Min'. Default '1Day'."
        )
      ),
      required = list("symbols")
    )
  )
)

#' All tool schemas bundled for ask_with_tools().
#' @export
TOOLS <- list(TOOL_SEARCH, TOOL_GET_BARS)

handle_search <- function(args) {
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

handle_get_bars_multi <- function(args) {
  symbols <- unlist(args$symbols, use.names = FALSE)
  days <- if (is.null(args$days)) 30 else as.integer(args$days)
  timeframe <- if (is.null(args$timeframe) || !nzchar(args$timeframe)) {
    "1Day"
  } else {
    args$timeframe
  }
  now <- lubridate$now(tzone = "UTC")
  then <- now - lubridate$ddays(days)
  fmt <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  bars <- market$get_bars_multi(
    symbols = symbols,
    timeframe = timeframe,
    start = fmt(then),
    end = fmt(now),
    feed = "iex"
  )
  return(bars)
}

#' Handlers bundled for ask_with_tools(). Names must match `function$name`.
#' @export
TOOL_HANDLERS <- list(
  search = handle_search,
  get_bars_multi = handle_get_bars_multi
)
