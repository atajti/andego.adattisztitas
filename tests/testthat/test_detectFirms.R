library(data.cleaning.HU)
context("c\u00E9gdetekt\u00E1l\u00E1s")

test_that("p\u00E9lda m\u0171k\u00F6dik", {
  expect_equal(detectFirms(toupper(unAccent(
               c("Andego Tan\u00E1csad\u00F3 Korl\u00E1tolt felel\u0151ss\u00E9g\u0171 t\u00E1rsas\u00E1g",
                 "Magyar Telekom Nyrt",
                 "Tajti Andr\u00E1s")))),
               c(TRUE, TRUE, FALSE))
  })