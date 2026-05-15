box::use(
  alpaca[
    get_api_keys,
    get_base_url,
    get_data_base_url,
    AlpacaMarketData
  ]
)

ALPACA_API_ENDPOINT <- Sys.getenv("ALPACA_API_ENDPOINT")
KEYS <- get_api_keys()

market <- AlpacaMarketData$new(
  keys = KEYS,
  base_url = TBASE,
  data_base_url = DBASE
)
