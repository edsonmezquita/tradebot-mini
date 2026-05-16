#' Used as an `ask_for_args` schema — model picks symbols/timeframe/days,
#' R does the actual fetching. There is no handler for this tool in the
#' agentic loop.
#' @export
TOOL_GET_BARS_MULTI <- list(
  type = "function",
  `function` = list(
    name = "get_bars_multi",
    description = paste(
      "Pick the right historical bar parameters for a set of tickers given",
      "the swing-trade horizon (1 day to 1 month) and each name's character.",
      "R will execute the actual fetch with the parameters you choose."
    ),
    parameters = list(
      type = "object",
      properties = list(
        symbols = list(
          type = "array",
          items = list(type = "string"),
          description = "Tickers to fetch, e.g. ['AAPL','MSFT','NVDA']"
        ),
        timeframe = list(
          type = "string",
          description = "Bar size: '1Day', '1Hour', '15Min', etc."
        ),
        days = list(
          type = "integer",
          description = "Days of history to pull, ending today."
        ),
        rationale = list(
          type = "string",
          description = "One short sentence on why you chose this timeframe and window for these names."
        )
      ),
      required = list("symbols", "timeframe", "days", "rationale")
    )
  )
)
