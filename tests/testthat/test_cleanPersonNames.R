library(data.cleaning.HU)
context("Szem\u00E9lynevek tiszt\u00EDt\u00E1sa")

test_that("p\u00E9ld\u00E1k m\u0171k\u00F6dnek-e", {
  expect_identical(cleanPersonNames("S\u00E1ndor J\u00F3zsef"),
                   "SANDOR JOZSEF")
  expect_identical(cleanPersonNames("S\u00E1ndor J\u00F3zsef Benedek"),
                   "SANDOR JOZSEF")
  expect_identical(cleanPersonNames("S. J\u00F3zsef Benedek"),
                   "S JOZSEF BENEDEK")
  })

test_that("NA \u00E9s hosszok stimmelnek", {
  expect_identical(cleanPersonNames(character(0)),
                   character(0))
  expect_identical(cleanPersonNames(NA),
                   NA_character_)
  })