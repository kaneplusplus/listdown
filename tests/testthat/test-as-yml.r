context("As YAML")

library(ggplot2)

source("make-reference.r")

if (make_reference) {
  dir.create("reference-data", showWarnings = FALSE)
}

cc_list <- list(
  Linear = ggplot(anscombe, aes(x = x1, y = y1)) + geom_point(),
  `Non Linear` = ggplot(anscombe, aes(x = x2, y = y2)) + geom_point(),
  `Outlier Vertical` = ggplot(anscombe, aes(x = x3, y = y3)) + geom_point(),
  `Outlier Horizontal` =  ggplot(anscombe, aes(x = x4, y = y4)) + geom_point(),
  a = list(b = 3, 4))

if (make_reference) {
  saveRDS(as_ld_yml(cc_list),
          file.path("reference-data", "as-yml.rds"))
}

expect_equal(as_ld_yml(cc_list),
             readRDS(file.path("reference-data", "as-yml.rds")))
