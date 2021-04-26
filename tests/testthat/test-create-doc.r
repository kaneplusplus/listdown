library(ggplot2)
source("reference.r")

cc <- list(
    iris = iris,
     Sepal.Length = list(
          Sepal.Width = ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
            geom_point(),
          Petal.Length = ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
            geom_point(),
     Colored = list(
          Sepal.Width = ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
            color = Species)) + geom_point(),
          Petal.Length = ggplot(iris, aes(x = Sepal.Length, y = Petal.Length,
            color = Species)) + geom_point())))

header <- ld_rmarkdown_header("Test header", author = "Some Dude",
                              date = "2020")

ld <- listdown(package = "ggplot2")

ldb <- ld_bundle_doc(cc, header, ld)

write_if_make_reference(ldb, "listdown-page-bundle.rds")

test_that("Create page bundle works", {
  expect_equal(ldb, read_reference("listdown-page-bundle.rds"))
})


test_that("A document has been created", {
  ldcp <- expect_warning(ld_create_doc(ldb, view = FALSE, quiet = TRUE))
  expect_s3_class(ldcp, "ld_page_bundle")
})

# This isn't working. It's probably a path issue.
#write_if_make_reference(ldcp, "listdown-page-bundle-after-creation.rds")
#
#test_that("Create page works.", {
#  expect_equal(ldcp,
#               read_reference("listdown-page-bundle-after-creation.rds"))
#})

saveRDS(cc, "cc.rds")

# This needs to be added correctly.
cc_path <- file.path(getwd(), "cc.rds")
#ldb <- ld_bundle_doc(readRDS(cc_path), header, ld)
#ld_create_doc(ldb, view = FALSE)

read_cc_path_str <- paste0('readRDS("', cc_path, '")')
ld_bundle_doc(read_cc_path_str, header, ld)

test_that("A document can be created.", {
  expect_warning(ld_create_doc(ldb, view = FALSE, quiet = TRUE))
})

unlink("cc.rds")
