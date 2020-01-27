library(ggplot2)
library(DT)

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

ld_register_libraries(libs = c("ggplot2", "DT"))
ld_register_decorators(list(ggplot = identity, data.frame = datatable))


