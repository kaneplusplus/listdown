context("Decorator Chunk Option")

source("make-reference.r")

pres_list <- list(iris = iris,
                  mtcars = mtcars)

saveRDS(pres_list, file.path("reference-data", "option-chunk-check.rds"))

expect_error(
  listdown(load_cc_expr = readRDS(file.path("reference-data", 
                                            "option-check.rds")),
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


if (make_reference) {
  saveRDS(ld_make_chunks(ld),
          file.path("reference-data", "chunk-decorator-option-1.rds"))
  saveRDS(capture.output(print(ld)),
          file.path("reference-data", "test-print-with-decorator.rds"))
}

expect_equal(ld_make_chunks(ld),
             readRDS(file.path("reference-data", 
                               "chunk-decorator-option-1.rds")))

expect_equal(capture.output(print(ld)),
             readRDS(file.path("reference-data", 
                               "test-print-with-decorator.rds")))
