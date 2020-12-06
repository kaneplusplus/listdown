
context("Chunk Options")

source("make-reference.r")

pres_list <- list(iris = iris,
                  mtcars = mtcars)

saveRDS(pres_list, file.path("reference-data", "option-check.rds"))

ld <- listdown(load_cc_expr = readRDS(file.path("reference-data", 
                                                "option-check.rds")),
               package = "knitr",
               decorator = list(data.frame = kable))

test_that("Only listdown objects can be used to make chunks.", {
  expect_error(ld_make_chunks(letters, 1:10))
})

if (make_reference) {
  saveRDS(ld_make_chunks(ld),
          file.path("reference-data", "chunk-option-1.rds"))
}

test_that("Output with no options works.", {
  expect_equal(ld_make_chunks(ld),
               readRDS(file.path("reference-data", "chunk-option-1.rds")))
})

pres_list$mtcars <- ld_chunk_opts(pres_list$mtcars,
                                  echo = FALSE, results = "as.is")

if (make_reference) {
  saveRDS(ld_make_chunks(ld),
          file.path("reference-data", "chunk-option-2.rds"))
}

test_that("Chunk options can be added.", {
  expect_equal(ld_make_chunks(ld),
               readRDS(file.path("reference-data", "chunk-option-2.rds")))
})

pres_list$iris <- ld_chunk_opts(pres_list$iris, chunk_name = "iris_chunk")

if (make_reference) {
  saveRDS(ld_make_chunks(ld),
          file.path("reference-data", "chunk-option-3.rds"))
}

test_that("Chunk names can be added.", {
  expect_equal(ld_make_chunks(ld),
               readRDS(file.path("reference-data", "chunk-option-3.rds")))
})

pres_list$iris <- ld_chunk_opts(pres_list$iris, echo = TRUE)

if (make_reference) {
  saveRDS(ld_make_chunks(ld),
          file.path("reference-data", "chunk-option-4.rds"))
}

test_that("Chunks can have names and options.", {
  expect_equal(ld_make_chunks(ld),
               readRDS(file.path("reference-data", "chunk-option-4.rds")))
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
