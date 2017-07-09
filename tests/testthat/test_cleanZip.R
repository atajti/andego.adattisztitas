library(andego.adattisztitas)
context("cleanZip")

test_that("hosszok_egyeznek",{
  expect_equal(length(cleanZip(character(0))),
               0)
  expect_equal(length(cleanZip("2011 Budakalász, Kinizsi utca 33.")),
               1)
  expect_equal(length(cleanZip(c("2011 Budakalász, Kinizsi utca 33.",
                                    "2011 Budakalász, Kinizsi utca 33."))),
               2)
  })

test_that("NA-k a helyükön vannak", {
  expect_equal(which(is.na(cleanZip(NA))),
               1)
  expect_equal(which(is.na(cleanZip(c("2011 Budakalász, Kinizsi utca 33.",
                                         NA)))),
               2)
  expect_equal(which(is.na(cleanZip(c(NA,
                                        "2011 Budakalász, Kinizsi utca 33.",
                                         NA)))),
               c(1, 3))
  })

test_that("működik a példa", {
  expect_equal(cleanZip(" 3120 Dorgalfalva, Kossuth utca 10."),
               "3120")
  })