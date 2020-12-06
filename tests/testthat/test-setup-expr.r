context("Setup expression.")

source("make-reference.r")

ld <- listdown(load_cc_expr = list(x = 1),
               setup_expr = knitr::opts_chunk$set(echo = FALSE))

if (make_reference) {
  saveRDS(paste(ld_make_chunks(ld), collapse = "\n"), 
          file.path("reference-data", "test-setup-expr.rds"))
  saveRDS(capture.output(print(ld)),
          file.path("reference-data", "test-print-ld.rds"))
}

expect_equal(readRDS(file.path("reference-data", "test-setup-expr.rds")),
             paste(ld_make_chunks(ld), collapse = "\n"))

expect_equal(capture.output(print(ld)),
             readRDS(file.path("reference-data", "test-print-ld.rds")))
