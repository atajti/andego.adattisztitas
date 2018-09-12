library(data.cleaning.HU)
context("cleanNames")

test_that("c\u00E9gek megtisztulnak", {
    expect_equal(cleanNames(toupper(unAccent(
               c("Andego Tan\u00E1csad\u00F3 Korl\u00E1tolt felel\u0151ss\u00E9g\u0171 t\u00E1rsas\u00E1g",
                 "Magyar Telekom Nyrt")))),
               c("ANDEGO TANACSADO KFT", "MAGYAR TELEKOM RT"))
})

test_that("mag\u00E1nszem\u00E9lyek megtisztulnak", {
  expect_identical(cleanNames("S\u00E1ndor J\u00F3zsef"),
                   "SANDOR JOZSEF")
  expect_identical(cleanNames("S\u00E1ndor J\u00F3zsef Benedek"),
                   "SANDOR JOZSEF")
  expect_identical(cleanNames("S. J\u00F3zsef Benedek"),
                   "S JOZSEF BENEDEK")
})