test_that("api_url is character", {
  expect_type(DataProvider$new()$api_url, "character")
})

test_that("describe_datasets return a tibble", {
  expect_true(inherits(DataProvider$new()$describe_datasets(), "tbl_df"), "tibble")
})