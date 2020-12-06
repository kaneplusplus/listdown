context("Write file")

library(ggplot2)

source("make-reference.r")

cc_list <- list(
  Linear = ggplot(anscombe, aes(x = x1, y = y1)) + geom_point(),
  `Non Linear` = ggplot(anscombe, aes(x = x2, y = y2)) + geom_point(),
  `Outlier Vertical` = ggplot(anscombe, aes(x = x3, y = y3)) + geom_point(),
  `Outlier Horizontal` =  ggplot(anscombe, aes(x = x4, y = y4)) + geom_point(),
  a = list(b = 3, 4))

rds_file <- "cc-list.rds"
saveRDS(cc_list, file = rds_file)

read_rds_str <- paste0("readRDS('", rds_file, "')")

ld <- listdown(load_cc_expr = readRDS("cc-list.rds"), package = "ggplot2")

ld_write_file(
  ld_rmarkdown_header(title = "The Anscombe Quartet",
                      author = "Francis Anscombe",
                      date = "1973"),
  ld,
  "anscombe-quartet.rmd")

if (make_reference) {
  ld_write_file(
    ld_rmarkdown_header(title = "The Anscombe Quartet",
                        author = "Francis Anscombe",
                        date = "1973"),
    ld,
    file.path("reference-data", "anscombe-quartet.rmd"))
}

expect_equal(readLines("anscombe-quartet.rmd"), 
             readLines(file.path("reference-data", "anscombe-quartet.rmd")))

unlink("anscombe-quartet.rmd")
unlink("cc-list.rds")

expect_error(
  ld_write_file(1:3, ld, file.path("reference-data", "anscombe-quartet.rmd")))

expect_error(
  ld_write_file(ld_rmarkdown_header(title = "test"), 1:3, "bunk"))


