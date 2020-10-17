## ----cc1----------------------------------------------------------------------
library(ggplot2)

library(listdown)

data(anscombe)

computational_components <- list(
  Linear = ggplot(anscombe, aes(x = x1, y = y1)) + geom_point(),
  `Non Linear` = ggplot(anscombe, aes(x = x2, y = y2)) + geom_point(),
  `Outlier Vertical`= ggplot(anscombe, aes(x = x3, y = y3)) + 
    geom_point(),
  `Outlier Horizontal` =  ggplot(anscombe, aes(x = x4, y = y4)) + 
    geom_point())

ld_cc_dendro(computational_components)


## -----------------------------------------------------------------------------
saveRDS(computational_components, "comp-comp.rds")

ld <- listdown(load_cc_expr = readRDS("comp-comp.rds"),
               package = "ggplot2")

ld


## ----eval = FALSE-------------------------------------------------------------
## doc <- c(
##   as.character(ld_rmarkdown_header("Anscombe's Quartet",
##                                    author = "Francis Anscombe",
##                                    date = "1973")),
##   ld_make_chunks(ld))
## 
## writeLines("anscome-example.rmd")
## 
## doc


## ----eval = TRUE, echo = FALSE------------------------------------------------
doc <- c(
  as.character(ld_rmarkdown_header("Anscombe's Quartet",
                                   author = "Francis Anscombe",
                                   date = "1973")),
  ld_make_chunks(ld))

doc


## -----------------------------------------------------------------------------
ld <- listdown(load_cc_expr = readRDS("comp-comp.rds"), 
               package = "ggplot2",
               echo = FALSE)

ld_make_chunks(ld)[1:7]


## ----results="as.is"----------------------------------------------------------
computational_components$Data <- anscombe
saveRDS(computational_components, "comp-comp.rds")
ld_make_chunks(ld)[32:36]


## -----------------------------------------------------------------------------
ld <- listdown(load_cc_expr = readRDS("comp-comp.rds"), 
               package = c("ggplot2", "DT"),
               decorator = list(data.frame = datatable))

ld_make_chunks(ld)[33:37]


## -----------------------------------------------------------------------------
comp_comp2 <- list(
  Iris = iris,
  Sepal.Length = list(
    Sepal.Width = ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
      geom_point(),
    Petal.Length = ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
      geom_point(),
    Colored = list(
      Sepal.Width = ggplot(iris, 
                          aes(x = Sepal.Length, y = Sepal.Width, 
                              color = Species)) + geom_point(),
      Petal.Length = ggplot(iris,
                            aes(x = Sepal.Length, y = Petal.Length, 
                                color = Species)) + geom_point())))

ld_cc_dendro(comp_comp2)


## -----------------------------------------------------------------------------
saveRDS(comp_comp2, "comp-comp2.rds")
ld <- listdown(load_cc_expr = readRDS("comp-comp2.rds"),
               package = c("ggplot2", "DT", "purrr"),
               decorator = list(ggplot = identity,
                                data.frame = datatable_no_search),
               init_expr = {
                 datatable_no_search <- partial(datatable,
                                                options = list(dom = 't'))
                 },
               echo = FALSE)

ld_make_chunks(ld)[2:10]


## -----------------------------------------------------------------------------
ld <- listdown(load_cc_expr = readRDS("comp-comp2.rds"),
               package = c("ggplot2", "DT", "purrr"),
               decorator_chunk_opts = 
                 list(ggplot = list(fig.width = 100,
                                    fig.height = 200)),
               init_expr = {
                 datatable_no_search <- partial(datatable,
                                                options = list(dom = 't'))
                 },
               echo = FALSE)

ld_make_chunks(ld)[c(12:16, 19:24)]


## -----------------------------------------------------------------------------
comp_comp2$Iris <- ld_chunk_opts(comp_comp2$Iris, echo = TRUE)
saveRDS(comp_comp2, "comp-comp2.rds")
ld_make_chunks(ld)[12:16]


## ---- eval = TRUE, message=FALSE, warning=FALSE-------------------------------
library(gtsummary)
library(dplyr)
library(survival)
library(survminer)
library(rmarkdown)

make_surv_cc <- function(trial, treat, surv_cond_chars) {
  table_1 <- trial %>%
    tbl_summary(by = all_of(treat)) %>%
    gtsummary::as_flextable()

  scs <- lapply(c("1", surv_cond_chars),
                function(sc) {
                  sprintf("Surv(ttdeath, death) ~ %s + %s", treat, sc) %>%
                    as.formula() %>%
                    surv_fit(trial) %>%
                    ggsurvplot()
                })
  names(scs) <- c("Overall", tools::toTitleCase(surv_cond_chars))
  list(`Table 1` = table_1, `Survival Plots` = scs)
}

surv_cc <- make_surv_cc(trial, treat = "trt",
                        surv_cond_chars = c("stage", "grade"))

ld_cc_dendro(surv_cc)


## ----eval = TRUE, message = FALSE, warning = FALSE----------------------------
class(surv_cc$`Survival Plots`$Overall) <- 
  class(surv_cc$`Survival Plots`$Stage) <-
  class(surv_cc$`Survival Plots`$Grade) <- "list"

names(surv_cc$`Survival Plots`) <- 
  paste(names(surv_cc$`Survival Plots`), "{.tabset}")

names(surv_cc$`Survival Plots`$`Overall {.tabset}`) <- 
  names(surv_cc$`Survival Plots`$`Stage {.tabset}`) <- 
  names(surv_cc$`Survival Plots`$`Grade {.tabset}`) <- 
  c("Plot", "Data", "Table")
  
saveRDS(surv_cc, "surv-cc.rds")

ld_surv <- listdown(load_cc_expr = readRDS("surv-cc.rds"),
                    package = c("gtsummary", "flextable", "DT", 
                                "ggplot2"),
                    decorator_chunk_opts = 
                      list(gg = list(fig.width = 8,
                                     fig.height = 6)),
                    decorator = list(data.frame = datatable),
                    echo = FALSE,
                    message = FALSE,
                    warning = FALSE,
                    fig.width = 7,
                    fig.height = 4.5)

writeLines(
  paste(c(
    as.character(ld_rmarkdown_header("Simple Trial Report")),
    ld_make_chunks(ld_surv))),
  "trial-report.rmd")

render("trial-report.rmd", quiet = TRUE)
browseURL("trial-report.html")

