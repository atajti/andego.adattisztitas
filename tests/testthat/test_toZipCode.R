library(data.cleaning.HU)
context("toZipCode")

test_that("hosszok_egyeznek",{
  expect_equal(length(toZipCode(character(0))),
               0)
  expect_equal(length(toZipCode("2011 Budakalász, Kinizsi utca 33.")),
               1)
  expect_equal(length(toZipCode(c("2011 Budakalász, Kinizsi utca 33.",
                                    "2011 Budakalász, Kinizsi utca 33."))),
               2)
  })

test_that("NA-k a helyükön vannak", {
  expect_equal(which(is.na(toZipCode(NA))),
               1)
  expect_equal(which(is.na(toZipCode(c("2011 Budakalász, Kinizsi utca 33.",
                                         NA)))),
               2)
  expect_equal(which(is.na(toZipCode(c(NA,
                                        "2011 Budakalász, Kinizsi utca 33.",
                                         NA)))),
               c(1, 3))
  })

test_that("működik a példa", {
  expect_equal(toZipCode(c("8000 Siófok, Ballagó utca 14/a",
                          "8000 Siofk, Ballagó utca 14/a 3. em 31.")),
               c("8000", "8000"))
  })