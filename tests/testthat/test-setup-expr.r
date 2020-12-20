context("Setup expression.")

source("reference.r")

ld <- listdown(load_cc_expr = list(x = 1),
               setup_expr = knitr::opts_chunk$set(echo = FALSE))

write_if_make_reference(paste(ld_make_chunks(ld), collapse = "\n"),
                        "test-setup-expr.rds")
write_if_make_reference(capture.output(print(ld)),
                        "test-print-ld.rds")

expect_equal(paste(ld_make_chunks(ld), collapse = "\n"),
             read_reference("test-setup-expr.rds"))

expect_equal(capture.output(print(ld)),
             read_reference("test-print-ld.rds"))
