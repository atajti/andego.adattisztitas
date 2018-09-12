library(data.cleaning.HU)
context("toZipCode")

test_that("hosszok_egyeznek",{
  expect_equal(length(toZipCode(character(0))),
               0)
  expect_equal(length(toZipCode("2011 Budakal\u00E1sz, Kinizsi utca 33.")),
               1)
  expect_equal(length(toZipCode(c("2011 Budakal\u00E1sz, Kinizsi utca 33.",
                                    "2011 Budakal\u00E1sz, Kinizsi utca 33."))),
               2)
  })

test_that("NA-k a hely\u00FCk\u00F6n vannak", {
  expect_equal(which(is.na(toZipCode(NA))),
               1)
  expect_equal(which(is.na(toZipCode(c("2011 Budakal\u00E1sz, Kinizsi utca 33.",
                                         NA)))),
               2)
  expect_equal(which(is.na(toZipCode(c(NA,
                                        "2011 Budakal\u00E1sz, Kinizsi utca 33.",
                                         NA)))),
               c(1, 3))
  })

test_that("m\u0171k\u00F6dik a p\u00E9lda", {
  expect_equal(toZipCode(c("8000 Si\u00F3fok, Ballag\u00F3 utca 14/a",
                          "8000 Siofk, Ballag\u00F3 utca 14/a 3. em 31.")),
               c("8000", "8000"))
  })