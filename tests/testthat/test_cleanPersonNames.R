library(data.cleaning.HU)
context("Személynevek tisztítása")

test_that("példák működnek-e", {
  expect_identical(cleanPersonNames("Sándor József"),
                   "SANDOR JOZSEF")
  expect_identical(cleanPersonNames("Sándor József Benedek"),
                   "SANDOR JOZSEF")
  expect_identical(cleanPersonNames("S. József Benedek"),
                   "S JOZSEF BENEDEK")
  })

test_that("NA és hosszok stimmelnek", {
  expect_identical(cleanPersonNames(character(0)),
                   character(0))
  expect_identical(cleanPersonNames(NA),
                   NA_character_)
  })