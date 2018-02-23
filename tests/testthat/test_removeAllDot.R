library(data.cleaning.HU)
context("removeAllDot")

test_that("hosszok_egyeznek",{
  expect_equal(length(removeAllDot(character(0))),
               0)
  expect_equal(length(removeAllDot("2011 Budakalász Kinizsi utca 33.")),
               1)
  expect_equal(length(removeAllDot(c("2011 Budakalász Kinizsi utca 33.",
                                    "2011 Budakalász Kinizsi utca 33."))),
               2)
  })

test_that("NA-k a helyükön vannak", {
  expect_equal(which(is.na(removeAllDot(NA))),
               1)
  expect_equal(which(is.na(removeAllDot(c("2011 Budakalász Kinizsi utca 33.",
                                         NA)))),
               2)
  expect_equal(which(is.na(removeAllDot(c(NA,
                                        "2011 Budakalász Kinizsi utca 33.",
                                         NA)))),
               c(1, 3))
  })

test_that("működik a példa", {
  expect_equal(removeAllDot("Az ellipszis argumentum: ..."),
               "Az ellipszis argumentum: ")
  })