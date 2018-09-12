library(data.cleaning.HU)
context("removeSpecials")

test_that("hosszok_egyeznek",{
  expect_equal(length(removeSpecials(character(0))),
               0)
  expect_equal(length(removeSpecials("2011 Budakal\u00E1sz Kinizsi utca 33.")),
               1)
  expect_equal(length(removeSpecials(c("2011 Budakal\u00E1sz Kinizsi utca 33.",
                                    "2011 Budakal\u00E1sz Kinizsi utca 33."))),
               2)
  })

test_that("NA-k a hely\u00FCk\u00F6n vannak", {
  expect_equal(which(is.na(removeSpecials(NA))),
               1)
  expect_equal(which(is.na(removeSpecials(c("2011 Budakal\u00E1sz Kinizsi utca 33.",
                                         NA)))),
               2)
  expect_equal(which(is.na(removeSpecials(c(NA,
                                        "2011 Budakal\u00E1sz Kinizsi utca 33.",
                                         NA)))),
               c(1, 3))
  })

test_that("m\u0171k\u00F6dik a p\u00E9lda", {
  expect_equal(removeSpecials("Uj\nsor"),
               "Ujsor")
  })