context("Decorator Chunk Option")

test_that("Decorator chunk options work.", {

  source("reference.r")

  write_if_make_reference( 
    list(iris = iris, mtcars = mtcars), 
    "option-chunk-check.rds")

  expect_error(
    listdown(load_cc_expr = readRDS(file.path("reference-data", 
                                              "option-chunk-check.rds")),
             package = "knitr",
             decorator = list(data.frame = kable),
             decorator_chunk_opts =
               list(data.frame = list("test_chunk", bunk = FALSE))))

  ld <- listdown(load_cc_expr = readRDS(file.path("reference-data",
                                                  "option-check.rds")),
                 package = "knitr",
                 decorator = list(data.frame = kable),
                 decorator_chunk_opts =
                   list(data.frame = list("test_chunk", echo = FALSE,
                                          results = "as.is")))


  write_if_make_reference(ld_make_chunks(ld), "chunk-decorator-option-1.rds")
  write_if_make_reference(capture.output(print(ld)), 
                          "test-print-with-decorator.rds")

  expect_equal(ld_make_chunks(ld),
               read_reference("chunk-decorator-option-1.rds"))
  expect_equal(capture.output(print(ld)),
               read_reference("test-print-with-decorator.rds"))

})
