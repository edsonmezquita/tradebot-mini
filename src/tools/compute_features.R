box::use(
  ../features[ FEATURE_REGISTRY, format_feature_menu ]
)

#' `ask_for_args` style — model picks indicator specs, R runs them
#' against the bars fetched in the previous stage. No handler.
#' @export
TOOL_COMPUTE_FEATURES <- list(
  type = "function",
  `function` = list(
    name = "compute_features",
    description = paste0(
      "Pick a tailored set of technical indicators (and parameters per indicator) ",
      "to compute on the bars previously fetched. Multi-instance same indicator ",
      "with different params is encouraged (e.g. rsi period=7 AND period=21 for ",
      "divergence). R will execute the computation.\n\n",
      "Available features (defaults shown — override any of them):\n",
      format_feature_menu()
    ),
    parameters = list(
      type = "object",
      properties = list(
        features = list(
          type = "array",
          description = paste(
            "List of feature spec objects. Example:",
            '[{"name":"rsi","period":14},',
            '{"name":"rsi","period":21},',
            '{"name":"macd","fast":12,"slow":26,"signal":9},',
            '{"name":"ema","periods":[9,21,50]}]'
          ),
          items = list(
            type = "object",
            properties = list(
              name = list(
                type = "string",
                description = "Indicator name from the menu.",
                enum = as.list(names(FEATURE_REGISTRY))
              ),
              period = list(type = "integer", description = "Lookback period (rsi/atr/bbands/supertrend)"),
              sd = list(type = "number", description = "Std-dev multiplier (bbands)"),
              fast = list(type = "integer", description = "Fast period (macd)"),
              slow = list(type = "integer", description = "Slow period (macd)"),
              signal = list(type = "integer", description = "Signal smoothing period (macd)"),
              multiplier = list(type = "number", description = "ATR multiplier (supertrend)"),
              periods = list(
                type = "array",
                items = list(type = "integer"),
                description = "List of EMA periods. Multiple periods also yield ema_cross_<short>_<long> signals."
              ),
              window = list(type = "integer", description = "Slope window (obv_slope)")
            ),
            required = list("name")
          )
        ),
        rationale = list(
          type = "string",
          description = "One short paragraph: why these indicators and these parameters for these names and this horizon."
        )
      ),
      required = list("features", "rationale")
    )
  )
)
