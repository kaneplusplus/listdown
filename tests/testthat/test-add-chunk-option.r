context("Chunk Options")

source("reference.r")

pres_list <- list(iris = iris, mtcars = mtcars)

write_if_make_reference(pres_list, "option-check.rds")

ld <- listdown(load_cc_expr = read_reference("option-check.rds"),
               package = "knitr",
               decorator = list(data.frame = kable))

test_that("Only listdown objects can be used to make chunks.", {
  expect_error(ld_make_chunks(letters, 1:10))
})

write_if_make_reference(ld_make_chunks(ld), "chunk-option-1.rds")

test_that("Output with no options works.", {
  expect_equal(ld_make_chunks(ld),
               read_reference("chunk-option-1.rds"))
})

pres_list$mtcars <- ld_chunk_opts(pres_list$mtcars,
                                  echo = FALSE, results = "as.is")

write_if_make_reference(ld_make_chunks(ld), "chunk-option-2.rds")

test_that("Chunk options can be added.", {
  expect_equal(ld_make_chunks(ld),
               read_reference("chunk-option-2.rds"))
})

pres_list$iris <- ld_chunk_opts(pres_list$iris, chunk_name = "iris_chunk")

write_if_make_reference(ld_make_chunks(ld), "chunk-option-3.rds")

test_that("Chunk names can be added.", {
  expect_equal(ld_make_chunks(ld),
               read_reference("chunk-option-3.rds"))
})

pres_list$iris <- ld_chunk_opts(pres_list$iris, echo = TRUE)

write_if_make_reference(ld_make_chunks(ld), "chunk-option-4.rds")

test_that("Chunks can have names and options.", {
  expect_equal(ld_make_chunks(ld),
               read_reference("chunk-option-4.rds"))
})

test_that("Bad options can't be added.", {
  expect_error(ld_chunk_opts(pres_list$iris, no_way_jose = FALSE))
})

test_that("Options can be NULL", {
  plt <- ld_chunk_opts(pres_list$iris, results = NULL)
  expect_equal(attributes(plt)$listdown$results, NULL)
})

test_that("Arg liststs can be created.", {
  chunk_opts <- list(echo = FALSE, eval = TRUE)
  plt <- ld_chunk_opts(pres_list$iris, chunk_opts = chunk_opts)
  attributes(plt)$listdown$chunk_name <- NULL
  expect_equal(attributes(plt)$listdown, list(echo = FALSE, eval = TRUE))
})

