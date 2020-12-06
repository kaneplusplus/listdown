context("Headers")

source("make-reference.r")

as.character(ld_rmarkdown_header("Workflow R", author = "Some Dude",
                              date = "2020"))

if (make_reference) {
  saveRDS(ld_rmarkdown_header("Workflow R", author = "Some Dude",
                              date = "2020"),
          file.path("reference-data", "test-rmarkdown-header.rds"))
}

mdh <- ld_rmarkdown_header("Workflow R", author = "Some Dude", date = "2020")
test_that("The R Markdown header hasn't changed.", {
  expect_equal(mdh,
               readRDS(file.path("reference-data",
                       "test-rmarkdown-header.rds")))
})

capture.output(mdh)

if (make_reference) {
  saveRDS(ld_workflowr_header("Workflow R"),
          file.path("reference-data", "test-workflowr-header.rds"))
}

wfr <- ld_workflowr_header("Workflow R")
test_that("The workflowrheader hasn't changed.", {
  expect_equal(wfr,
               readRDS(file.path("reference-data",
                       "test-workflowr-header.rds")))
})
