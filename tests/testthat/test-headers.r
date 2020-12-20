context("Headers")

source("reference.r")

mdh <- ld_rmarkdown_header("Workflow R", author = "Some Dude", date = "2020")

write_if_make_reference(mdh, "test-rmarkdown-header.rds")

test_that("The R Markdown header hasn't changed.", {
  expect_equal(mdh, read_reference("test-rmarkdown-header.rds"))
})

wfr <- ld_workflowr_header("Workflow R")

write_if_make_reference(wfr, "test-workflowr-header.rds")

test_that("The workflowrheader hasn't changed.", {
  expect_equal(wfr, read_reference("test-workflowr-header.rds"))
})
