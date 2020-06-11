context("Decorator Chunk Option")

source("make-reference.r")

if (make_reference) {
  dir.create("reference-data", showWarnings = FALSE)
}

pres_list <- list(iris = iris,
                  mtcars = mtcars)

saveRDS(pres_list, "reference-data/option-chunk-check.rds")

expect_error(
  listdown(readRDS("reference-data/option-check.rds"),
           package = "knitr",
           decorator = list(data.frame = kable),
           decorator_chunk_opts =
             list(data.frame = list("test_chunk", bunk = FALSE))))

ld <- listdown(readRDS("reference-data/option-check.rds"),
               package = "knitr",
               decorator = list(data.frame = kable),
               decorator_chunk_opts =
                 list(data.frame = list("test_chunk", echo = FALSE,
                                        results = "as.is")))


if (make_reference) {
  saveRDS(ld_make_chunks(ld),
          file.path("reference-data", "chunk-decorator-option-1.rds"))
}

expect_equal(ld_make_chunks(ld),
             readRDS(file.path("reference-data", 
                               "chunk-decorator-option-1.rds")))
