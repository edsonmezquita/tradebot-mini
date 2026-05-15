box::use(
  httr2
)

#' Call the DeepSeek chat completions API.
#'
#' DeepSeek exposes an OpenAI-compatible endpoint at
#' https://api.deepseek.com/chat/completions.
#'
#' @param prompt User message (string).
#' @param system Optional system prompt to steer the model.
#' @param model DeepSeek model ID. Default "deepseek-v4-flash" — 1M context,
#'   cheapest tier ($0.14/$0.28 per 1M tokens in/out). "deepseek-v4-pro" is
#'   the higher-quality 1M-context option.
#' @param temperature Sampling temperature (0-2).
#' @param max_tokens Cap on response length. Note: V4 models are "thinking"
#'   models — internal reasoning tokens count against this cap, so set it
#'   high enough (>= a few hundred) or `content` may come back empty.
#' @param api_key DeepSeek API key. Defaults to env var DEEPSEEK_KEY.
#' @param timeout Request timeout in seconds.
#' @return The assistant's reply as a single string.
#' @export
ask <- function(
  prompt,
  system = NULL,
  model = "deepseek-v4-flash",
  temperature = 0.7,
  max_tokens = NULL,
  api_key = Sys.getenv("DEEPSEEK_KEY"),
  timeout = 60
) {
  stopifnot(nzchar(prompt), nzchar(api_key))

  messages <- list()
  if (!is.null(system) && nzchar(system)) {
    messages <- c(messages, list(list(role = "system", content = system)))
  }
  messages <- c(messages, list(list(role = "user", content = prompt)))

  body <- list(
    model = model,
    messages = messages,
    temperature = temperature,
    max_tokens = max_tokens,
    stream = FALSE
  )
  body <- body[!vapply(body, is.null, logical(1))]

  resp <- httr2$request("https://api.deepseek.com") |>
    httr2$req_url_path("/chat/completions") |>
    httr2$req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2$req_body_json(body) |>
    httr2$req_timeout(timeout) |>
    httr2$req_user_agent("tradebot-mini/0.1") |>
    httr2$req_perform()

  parsed <- httr2$resp_body_json(resp, simplifyVector = FALSE)
  return(parsed$choices[[1]]$message$content)
}
