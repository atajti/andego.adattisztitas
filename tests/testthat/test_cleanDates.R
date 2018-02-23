library(data.cleaning.HU)
context("cleanDates")

test_that("hosszok_egyeznek",{
  expect_equal(length(cleanDates(character(0))),
               0)
  expect_equal(length(cleanDates("2017. 07. 10")),
               1)
  expect_equal(length(cleanDates(c("2017. 07. 10",
                                    "2017. 07. 10"))),
               2)
  })

test_that("NA-k a helyükön vannak", {
  expect_equal(which(is.na(cleanDates(NA))),
               1)
  expect_equal(which(is.na(cleanDates(c("2017. 07. 10",
                                         NA)))),
               2)
  expect_equal(which(is.na(cleanDates(c(NA,
                                        "2017. 07. 10",
                                         NA)))),
               c(1, 3))
  })

test_that("működik a példa", {
  expect_equal(cleanDates("56-10-23"),
               as.Date("1956-10-23"))
  })