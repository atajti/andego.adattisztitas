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

test_that("NA-k a helyükön vannak", {
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

test_that("működik a példa", {
  expect_equal(cleanPhones("+36 (30) 1010101"),
               "36301010101")
  })