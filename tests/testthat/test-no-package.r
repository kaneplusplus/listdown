context("No packages need to be specified.")

source("reference.r")

ld <- listdown(load_cc_expr = list(x = 1))

if (make_reference) {
  saveRDS(paste(ld_make_chunks(ld), collapse = "\n"), 
          file.path("reference-data", "test-no-package.rds"))
}

write_if_make_reference(paste(ld_make_chunks(ld), collapse = "\n"),
                        "test-no-package.rds")

test_that("No packages need to be specified.", {
  expect_equal(paste(ld_make_chunks(ld), collapse = "\n"),
               read_reference("test-no-package.rds"))
})

