library(data.cleaning.HU)
context("cleanEmails")

test_that("hosszok_egyeznek",{
  expect_equal(length(cleanEmails(character(0))),
               0)
  expect_equal(length(cleanEmails("vmi@email.cimem")),
               1)
  expect_equal(length(cleanEmails(c("vmi@email.cim",
                                    "vmi@email.cim"))),
               2)
  })

test_that("NA-k a hely\u00FCk\u00F6n vannak", {
  expect_equal(which(is.na(cleanEmails(NA))),
               1)
  expect_equal(which(is.na(cleanEmails(c("vmi@email.cim",
                                         NA)))),
               2)
  expect_equal(which(is.na(cleanEmails(c(NA,
                                        "vmi@email.cim",
                                         NA)))),
               c(1, 3))
  })

test_that("m\u0171k\u00F6dik a p\u00E9lda", {
  expect_equal(cleanEmails("vmi@email.cim"),
               "vmi@email.cim")
  })