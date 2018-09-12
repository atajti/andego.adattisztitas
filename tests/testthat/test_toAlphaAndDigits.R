library(data.cleaning.HU)
context("toAlphaAndDigits")

test_that("hosszok_egyeznek",{
  expect_equal(length(toAlphaAndDigits(character(0))),
               0)
  expect_equal(length(toAlphaAndDigits("2011 Budakal\u00E1sz, Kinizsi utca 33.")),
               1)
  expect_equal(length(toAlphaAndDigits(c("2011 Budakal\u00E1sz, Kinizsi utca 33.",
                                    "2011 Budakal\u00E1sz, Kinizsi utca 33."))),
               2)
  })

test_that("NA-k a hely\u00FCk\u00F6n vannak", {
  expect_equal(which(is.na(toAlphaAndDigits(NA))),
               1)
  expect_equal(which(is.na(toAlphaAndDigits(c("2011 Budakal\u00E1sz, Kinizsi utca 33.",
                                         NA)))),
               2)
  expect_equal(which(is.na(toAlphaAndDigits(c(NA,
                                        "2011 Budakal\u00E1sz, Kinizsi utca 33.",
                                         NA)))),
               c(1, 3))
  })

test_that("m\u0171k\u00F6dik a p\u00E9lda", {
  expect_equal(toAlphaAndDigits("1+4=\u00F6t"),
               "14\u00F6t")
  })