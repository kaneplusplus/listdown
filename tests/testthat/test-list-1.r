library(ggplot2)
library(DT)

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
ld <- listdown(libs = c("ggplot2", "DT"), rds_loc = "here('a/file')",
               decorators = list(ggplot = identity, data.frame = datatable))

# Create the RMarkdown string.
ld_render(test_list, ld)
