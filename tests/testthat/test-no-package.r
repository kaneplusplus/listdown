context("No packages can be specified.")

source("make-reference.r")

ld <- listdown(load_cc_expr = list(x = 1))

if (make_reference) {
  saveRDS(paste(ld_make_chunks(ld), collapse = "\n"), 
          file.path("reference-data", "test-no-package.rds"))
}

expect_equal(readRDS(file.path("reference-data", "test-no-package.rds")),
             paste(ld_make_chunks(ld), collapse = "\n"))

