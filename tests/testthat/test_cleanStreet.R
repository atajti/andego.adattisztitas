library(data.cleaning.HU)
context("Utcatiszt\u00EDt\u00E1s")

test_that("hosszok_egyeznek",{
  expect_equal(length(cleanStreet(character(0))),
               0)
  expect_equal(length(cleanStreet("2011 Budakal\u00E1sz Kinizsi utca 33.")),
               1)
  expect_equal(length(cleanStreet(c("2011 Budakal\u00E1sz Kinizsi utca 33.",
                                    "2011 Budakal\u00E1sz Kinizsi utca 33."))),
               2)
  })

test_that("NA-k a hely\u00FCk\u00F6n vannak", {
  expect_equal(which(is.na(cleanStreet(NA))),
               1)
  expect_equal(which(is.na(cleanStreet(c("2011 Budakal\u00E1sz Kinizsi utca 33.",
                                         NA)))),
               2)
  expect_equal(which(is.na(cleanStreet(c(NA,
                                        "2011 Budakal\u00E1sz Kinizsi utca 33.",
                                         NA)))),
               c(1, 3))

  })