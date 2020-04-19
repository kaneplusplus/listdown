library(ggplot2)
library(DT)

context("Headers")

source("make-reference.r")

if (make_reference) {
  dir.create("reference-data", showWarnings = FALSE)
}

if (make_reference) {
  saveRDS(ld_workflowr_header("Workflow R"),
          file.path("reference-data", "test-workflowr-header.rds"))
}

test_that("The workflowr header hasn't changed.", {
  expect_equal(ld_workflowr_header("Workflow R"),
               readRDS(file.path("reference-data", 
                       "test-workflowr-header.rds")))
})

