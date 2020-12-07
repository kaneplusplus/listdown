context("Not list decorators")

expect_error(listdown(load_cc_expr = list(x = 1),
                      setup_expr = knitr::opts_chunk$set(echo = FALSE),
                      decorator = list(list = print)))

