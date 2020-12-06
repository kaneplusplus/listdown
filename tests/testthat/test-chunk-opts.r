
context("Chunk Options")

library(knitr)

ld <- listdown(load_cc_expr = readRDS(file.path("reference-data", 
                                                "option-check.rds")),
               package = "knitr",
               decorator = list(data.frame = kable))

test_that("Bad chunk options fail.", {
  expect_error(listdown(readRDS(file.path("reference-data", 
                                          "option-check.rds")),
               package = "knitr",
               decorators = list(data.frame = kable),
               bunk = letters))
})
