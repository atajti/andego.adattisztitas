library(data.cleaning.HU)
context("cleanPhones")

test_that("hosszok_egyeznek",{
  expect_equal(length(cleanPhones(character(0))),
               0)
  expect_equal(length(cleanPhones("+36 (30) 1010101")),
               1)
  expect_equal(length(cleanPhones(c("+36 (30) 1010101",
                                    "+36 (30) 1010101"))),
               2)
  })

test_that("NA-k a hely\u00FCk\u00F6n vannak", {
  expect_equal(which(is.na(cleanPhones(NA))),
               1)
  expect_equal(which(is.na(cleanPhones(c("+36 (30) 1010101",
                                         NA)))),
               2)
  expect_equal(which(is.na(cleanPhones(c(NA,
                                        "+36 (30) 1010101",
                                         NA)))),
               c(1, 3))
  })

test_that("m\u0171k\u00F6dik a p\u00E9lda", {
  expect_equal(cleanPhones("+36 (30) 1010101"),
               "36301010101")
  })