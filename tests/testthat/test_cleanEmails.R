library(andego.adattisztitas)
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

test_that("NA-k a helyükön vannak", {
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

test_that("működik a példa", {
  expect_equal(cleanEmails("vmi@email.cim"),
               "vmi@email.cim")
  })