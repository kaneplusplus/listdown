context("Dendrogram")

library(ggplot2)

test_that("Dendrograms work.", {

  source("reference.r")

  cc_list <-
    list(
      Linear = ggplot(anscombe, aes(x = x1, y = y1)) + geom_point(),
      `Non Linear` = ggplot(anscombe, aes(x = x2, y = y2)) + geom_point(),
      `Outlier Vertical` = ggplot(anscombe, aes(x = x3, y = y3)) + geom_point(),
      `Outlier Horizontal` =  ggplot(anscombe, aes(x = x4, y = y4)) +
        geom_point()
    )

  write_if_make_reference(ld_cc_dendro(cc_list), "cc-dendro.rds")

  expect_equal(ld_cc_dendro(cc_list),
               read_reference("cc-dendro.rds"))

  expect_error(ld_cc_dendro(exp))

  ldc <- ld_cc_dendro(cc_list)

  pldc <- capture.output(print(ldc))

  expect_equal(unclass(pldc), c("", unclass(ldc), ""))

  # Should there be a test here?

  pldc <- capture.output(ld_cc_dendro(list(3)))

})
