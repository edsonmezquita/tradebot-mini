#' `ask_for_args` style — model emits a batch of trade decisions, R places
#' each via Alpaca's add_order. Use action="hold" for no-op.
#' @export
TOOL_SUBMIT_TRADES <- list(
  type = "function",
  `function` = list(
    name = "submit_trades",
    description = paste(
      "Submit a batch of trade decisions for the current cycle. R will execute",
      "each non-hold decision as a market order via Alpaca. One entry per ticker.",
      "Use 'hold' to do nothing for that ticker. Notional is the dollar amount",
      "(required for buy/sell, ignored for hold)."
    ),
    parameters = list(
      type = "object",
      properties = list(
        decisions = list(
          type = "array",
          description = "One decision per ticker considered this cycle.",
          items = list(
            type = "object",
            properties = list(
              ticker = list(
                type = "string",
                description = "Ticker symbol exactly as fetched (e.g. 'AAPL')."
              ),
              action = list(
                type = "string",
                enum = list("buy", "sell", "hold"),
                description = "Trade direction. 'hold' means no order."
              ),
              notional = list(
                type = "number",
                description = "Dollar amount for the order. Required for buy/sell."
              ),
              confidence = list(
                type = "number",
                description = "Your confidence in this decision, 0.0 to 1.0."
              ),
              reason = list(
                type = "string",
                description = "One sentence grounded in indicators + account context."
              )
            ),
            required = list("ticker", "action", "confidence", "reason")
          )
        )
      ),
      required = list("decisions")
    )
  )
)
