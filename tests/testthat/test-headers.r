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

if (make_reference) {
  saveRDS(ld_rmarkdown_header("R Markdown", "Author", "Date"),
          file.path("reference-data", "test-rmarkdown-header.rds"))
}

test_that("The R Markdown header hasn't changed.", {
  expect_equal(ld_rmarkdown_header("R Markdown", "Author", "Date"),
               readRDS(file.path("reference-data", 
                       "test-rmarkdown-header.rds")))
})

if (make_reference) {
  saveRDS(ld_ioslides_header("R Markdown", "Author", "Date"),
          file.path("reference-data", "test-ioslides-header.rds"))
}

test_that("The ioslides header hasn't changed.", {
  expect_equal(ld_ioslides_header("R Markdown", "Author", "Date"),
               readRDS(file.path("reference-data", 
                       "test-ioslides-header.rds")))
})

