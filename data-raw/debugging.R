## code to prepare `debugging` dataset goes here
req <- GET("https://api.github.com/orgs/open-numbers/repos")
response <- content(req)

debugging <- rbindlist(
  lapply(response, function(x) {
    as.data.table(
      x[lengths(x) == 1],
      lapply(x[lengths(x) != 1], function(x) {list(as.data.table(x))})
    )
  }),
  fill = TRUE
)[]

usethis::use_data(debugging, overwrite = TRUE)
