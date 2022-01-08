library(ggplot2)

ld <- listdown(package = "ggplot2")

iris_cc <- list(
    iris = iris,
     Sepal.Length = list(
          Sepal.Width = 
            ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
              geom_point(),
          Petal.Length = 
            ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
              geom_point(),
     Colored = list(
          Sepal.Width = 
            ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                   color = Species)) + 
              geom_point(),
          Petal.Length = 
            ggplot(iris, aes(x = Sepal.Length, y = Petal.Length,
                   color = Species)) + 
              geom_point())))

header <- ld_rmarkdown_header("Iris", author = "Some Dude", date = "2020")

iris_page <- ld_bundle_doc(iris_cc, header, ld) 

anscombe_cc <- list(
  Linear = 
    ggplot(anscombe, aes(x = x1, y = y1)) + 
      geom_point() + 
      theme_bw(),
  `Non Linear` = 
    ggplot(anscombe, aes(x = x2, y = y2)) + 
      geom_point() + 
      theme_bw(),
  `Outlier Vertical`= 
    ggplot(anscombe, aes(x = x3, y = y3)) + 
      geom_point() + 
      theme_bw(),
  `Outlier Horizontal` = 
    ggplot(anscombe, aes(x = x4, y = y4)) + 
      geom_point() + 
      theme_bw())

header <- ld_rmarkdown_header("Anscombe", author = "Anscombe", date = "2020")

anscombe_page <- ld_bundle_doc(anscombe_cc, header, ld) 

pages <- list(Anscombe = anscombe_page, Iris = iris_page)

#test_that("Site can be built.", {
#  site_yaml <- 
#    ld_site_yaml(
#      "Test Site", 
#      tab_name = names(pages),
#      rmd_name = c("index.Rmd", "iris.Rmd"))
#
#  site_path <- ld_build_html_site(
#    pages,
#    site_yaml,
#    view = TRUE,
#    quiet = TRUE)
#  expect_equal(basename(site_path), "index.html")
#})

