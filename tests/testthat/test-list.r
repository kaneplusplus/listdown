context("Write file")

library(ggplot2)

source("reference.r")

cc_list <- list(
  Linear = list(
    ggplot(anscombe, aes(x = x1, y = y1)) + geom_point(),
    iris[1:10,]))

rds_file <- "cc-list-2.rds"

saveRDS(cc_list, file = rds_file)

ld <- listdown(load_cc_expr = readRDS("cc-list-2.rds"), package = "ggplot2")

test <- ld_make_chunks(ld)

write_if_make_reference(test, "ld-cc-list-2-output.rds")

expect_equal(test, read_reference("ld-cc-list-2-output.rds"))

unlink("cc-list-2.rds")
