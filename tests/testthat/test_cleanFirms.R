library(data.cleaning.HU)
context("c\u00E9gnevek tiszt\u00EDt\u00E1sa")

test_that("p\u00E9lda m\u0171k\u00F6dik", {
  expect_equal(cleanFirms(toupper(unAccent(
               c("Andego Tan\u00E1csad\u00F3 Korl\u00E1tolt felel\u0151ss\u00E9g\u0171 t\u00E1rsas\u00E1g",
                 "Magyar Telekom Nyrt")))),
               c("ANDEGO TANACSADO KFT", "MAGYAR TELEKOM RT"))
  })

test_that("nem nagybet\u0151s-\u00E9kezettel\u00EDtett neveken m\u0171k\u00F6dik",
  expect_equal(cleanFirms("GVC George's Venture Capital Z\u00E1rtk\u00F6r\u0171en M\u0171k\u00F6d\u0151 R\u00E9szv\u00E9nyt\u00E1rsas\u00E1g"),
               "GVC GEORGE'S VENTURE CAPITAL RT"))

test_that("&amp; kicser\u00E9l\u0151dik",
  expect_equal(cleanFirms("K&amp;H ZRT"),
               "K&H RT"))
