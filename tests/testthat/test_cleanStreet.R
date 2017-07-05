library(andego.adattisztitas)
context("Utcatisztítás")

test_that("hosszok_egyeznek",{
  expect_equal(length(cleanStreet(character(0))),
               0)
  expect_equal(length(cleanStreet("vmilyen utca")),
               1)
  expect_equal(length(cleanStreet(c("vmilyen utca", "vmilyen utca"))),
               2)
  })

test_that("NA-k a helyükön vannak", {
  expect_equal(which(is.na(cleanStreet(NA))),
               1)
  expect_equal(which(is.na(cleanStreet(c("vmilyen utca", NA)))),
               2)
  expect_equal(which(is.na(cleanStreet(c(NA, "vmilyen utca", NA)))),
               c(1, 3))

  })