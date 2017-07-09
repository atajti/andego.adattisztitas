library(andego.adattisztitas)
context("toAlphaAndDigits")

test_that("hosszok_egyeznek",{
  expect_equal(length(toAlphaAndDigits(character(0))),
               0)
  expect_equal(length(toAlphaAndDigits("2011 Budakalász, Kinizsi utca 33.")),
               1)
  expect_equal(length(toAlphaAndDigits(c("2011 Budakalász, Kinizsi utca 33.",
                                    "2011 Budakalász, Kinizsi utca 33."))),
               2)
  })

test_that("NA-k a helyükön vannak", {
  expect_equal(which(is.na(toAlphaAndDigits(NA))),
               1)
  expect_equal(which(is.na(toAlphaAndDigits(c("2011 Budakalász, Kinizsi utca 33.",
                                         NA)))),
               2)
  expect_equal(which(is.na(toAlphaAndDigits(c(NA,
                                        "2011 Budakalász, Kinizsi utca 33.",
                                         NA)))),
               c(1, 3))
  })

test_that("működik a példa", {
  expect_equal(toAlphaAndDigits("1+4=öt"),
               "14öt")
  })