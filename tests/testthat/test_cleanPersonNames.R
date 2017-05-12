library(andego.adattisztitas)
context("Személynevek tisztítása")

test_that("példák működnek-e", {
  expect_identical(cleanPersonNames("Sándor József"),
                   "SANDOR JOZSEF")
  expect_identical(cleanPersonNames("Sándor József Benedek"),
                   "SANDOR J BENEDEK")
  expect_identical(cleanPersonNames("S. József Benedek"),
                   "S JOZSEF BENEDEK")
  })