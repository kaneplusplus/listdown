
context("Chunk Options")

test_that("Bad chunk options fail.", {
  expect_error(listdown(readRDS(file.path("reference-data", 
                                          "option-check.rds")),
               package = "knitr",
               decorators = list(data.frame = kable),
               bunk = letters))
})
