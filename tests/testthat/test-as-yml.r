context("As YML")

library(ggplot2)

test_that("Writing a yml file works.", {

  source("reference.r")

  cc_list <- list(
    Linear = ggplot(anscombe, aes(x = x1, y = y1)) + 
      geom_point(),
    `Non Linear` = ggplot(anscombe, aes(x = x2, y = y2)) + geom_point(),
    `Outlier Vertical` = ggplot(anscombe, aes(x = x3, y = y3)) + geom_point(),
    `Outlier Horizontal` =  ggplot(anscombe, aes(x = x4, y = y4)) + 
      geom_point(),
    a = list(b = 3, 4))

  write_if_make_reference(as_ld_yml(cc_list), "as-yml.rds")

  expect_equal(as_ld_yml(cc_list),
               read_reference("as-yml.rds"))
})
