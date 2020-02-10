library(ggplot2)
library(DT)

context("Make Chunks")

source("make-reference.r")

if (make_reference) {
  dir.create("reference-data", showWarnings = FALSE)
}

# The list we'll make an RMarkdown document from.
test_list <- list(iris = iris,
     list(Sepal.Length =
          list(Sepal.Width = ggplot(iris,
                                    aes(x = Sepal.Length, y = Sepal.Width)) +
                 geom_point()),
          list(Petal.Length = ggplot(iris,
                                     aes(x = Sepal.Length, y = Sepal.Width))),
          list(Colored = list(Sepal.Width = ggplot(iris,
                                    aes(x = Sepal.Length, y = Sepal.Width,
                                        color = Species)) +
                                geom_point(),
                              Petal.Length = ggplot(iris,
                                    aes(x = Sepal.Length, y = Petal.Length,
                                        color = Species)) +
                                geom_point()))))

# The listdown object. It needs the libraries to load, the decorators,
# and, coming soon, arbitrary code.
# If it sees a ggplot object, it does nothing. If it sees a dataframe
# it decorates with datatable from the DT package.

ld <- listdown(load_ld_expr = readRDS("a/file"),
               package = c("ggplot2", "DT", "purrr"),
               decorator = list(ggplot = identity,
                                data.frame = datatable),
               init_expr = {
                 datatable <- partial(DT::datatable,
                                      options = list(ordering = FALSE))})

test_that("A listdown object can be created.", {
  expect_true(inherits(ld, "listdown"))
})

# Create the RMarkdown string.

if (make_reference) {
  saveRDS(ld_make_chunks(test_list, ld),
           file.path("reference-data", "test-make-chunks-1.rds"))
}

test_that("The listdown object is the same as before.", {
  expect_equal(ld_make_chunks(test_list, ld),
               readRDS(file.path("reference-data", "test-make-chunks-1.rds")))
})

ld2 <- listdown(load_ld_expr = readRDS("a/file"),
                package = c("ggplot2", "DT", "purrr"),
                decorator = list(ggplot = identity,
                                  data.frame = datatable),
                init_expr = {
                  datatable <- partial(DT::datatable,
                                       options = list(ordering = FALSE))},
                echo = FALSE,
                warning = FALSE,
                message = FALSE)

test_that("A listdown object with initial expression can be created.", {
  expect_true(inherits(ld2, "listdown"))
})

# Create the RMarkdown string.

if (make_reference) {
  saveRDS(ld_make_chunks(test_list, ld2),
           file.path("reference-data", "test-make-chunks-2.rds"))
}

test_that("The listdown object with init expr is the same as before.", {
  expect_equal(ld_make_chunks(test_list, ld2),
               readRDS(file.path("reference-data", "test-make-chunks-2.rds")))
})
