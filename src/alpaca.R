box::use(
  alpaca[
    get_api_keys,
    get_base_url,
    get_data_base_url,
    AlpacaMarketData
  ]
)

KEYS <- get_api_keys()
ALPACA_DATA_ENDPOINT <- get_data_base_url()
ALPACA_API_ENDPOINT <- get_base_url()

market <- AlpacaMarketData$new(
  keys = KEYS,
  base_url = ALPACA_API_ENDPOINT,
  data_base_url = ALPACA_DATA_ENDPOINT
)
