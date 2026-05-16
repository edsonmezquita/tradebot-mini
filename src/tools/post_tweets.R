#' `ask_for_args` style — model emits 1 status tweet + up to 2 replies,
#' R posts each via the twitter module.
#' @export
TOOL_POST_TWEETS <- list(
  type = "function",
  `function` = list(
    name = "post_tweets",
    description = paste(
      "Submit a batch of tweets to post via the bot's account (Chris de la Thune).",
      "Up to 3 tweets per call: exactly ONE status update (no in_reply_to) plus",
      "OPTIONALLY 0-2 replies to mentions you actually want to engage with.",
      "Skip replies entirely if no mention is worth answering (trolls, spam, low-effort).",
      "Stay in character: confident, precise, slightly smug.",
      "Each tweet max 280 chars. Disclaimer 'not financial advice' should appear at",
      "least once across the batch (typically the status update)."
    ),
    parameters = list(
      type = "object",
      properties = list(
        tweets = list(
          type = "array",
          description = "1-3 tweet objects. Exactly one must have no in_reply_to_tweet_id (the status update).",
          items = list(
            type = "object",
            properties = list(
              text = list(
                type = "string",
                description = "Tweet body, max 280 chars including emoji."
              ),
              in_reply_to_tweet_id = list(
                type = "string",
                description = "ID of the mention you're replying to. OMIT for the status update tweet."
              ),
              reason = list(
                type = "string",
                description = "One short phrase: why you wrote this tweet (logged, not posted)."
              )
            ),
            required = list("text", "reason")
          )
        )
      ),
      required = list("tweets")
    )
  )
)
