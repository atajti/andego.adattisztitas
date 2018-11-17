library(data.cleaning.HU)
context("c\u00EDmtiszt\u00EDt\u00E1s")

test_that("\u00E9kezetes v\u00E1rosnevek", {
  expect_equal(cleanAddress(
                 "2011 Budakal\u00E1sz Kinizsi utca 33."),
               "2011 BUDAKALASZ KINIZSI 33")
  expect_equal(cleanAddress(
                 "2011 BUDAKAL\u00C1SZ Kinizsi utca 33."),
               "2011 BUDAKALASZ KINIZSI 33")
  expect_equal(cleanAddress(
                 "6000 Kecskem\u00E9t Tat\u00E1r sor 22"),
               "6000 KECSKEMET TATAR 22")
  expect_equal(cleanAddress(
                 "6000 KECSKEM\u00C9T Tat\u00E1r sor 22"),
                 "6000 KECSKEMET TATAR 22")
  })

test_that("k\u00F6zter\u00FCletek", {
  expect_equal(cleanAddress(
                 "2011 Budakal\u00E1sz Kinizsi u. 33."),
               "2011 BUDAKALASZ KINIZSI 33")
  expect_equal(cleanAddress(
                 "2011 Budakal\u00E1sz Kinizsi utca 33."),
               "2011 BUDAKALASZ KINIZSI 33")
  expect_equal(cleanAddress(
                 "6000 Kecskem\u00E9t Tat\u00E1r sor 22"),
               "6000 KECSKEMET TATAR 22")
  expect_equal(cleanAddress(
                 "6000 Kecskem\u00E9t Tat\u00E1r t\u00E9r 22"),
               "6000 KECSKEMET TATAR 22")
  expect_equal(cleanAddress(
                 "6000 Kecskem\u00E9t Tat\u00E1r \u00FAt 22"),
               "6000 KECSKEMET TATAR 22")
  expect_equal(cleanAddress(
                 "6000 Kecskem\u00E9t Tat\u00E1r k\u00F6z 22"),
               "6000 KECSKEMET TATAR 22")
  })

test_that("bonyolult lak\u00E1sc\u00EDmek", {
  expect_equal(cleanAddress(
                 "6000 Kecskem\u00E9t Tat\u00E1r k\u00F6z 22"),
               "6000 KECSKEMET TATAR 22")
  expect_equal(cleanAddress(
                 "6000 Kecskem\u00E9t Tat\u00E1r k\u00F6z 22/b"),
               "6000 KECSKEMET TATAR 22")
  expect_equal(cleanAddress(
                 "6000 Kecskem\u00E9t Tat\u00E1r k\u00F6z 22-23"),
               "6000 KECSKEMET TATAR 22")
  expect_equal(cleanAddress(
                 "6000 Kecskem\u00E9t Tat\u00E1r k\u00F6z 22. 1. em. 1."),
                 "6000 KECSKEMET TATAR 22")
  expect_equal(cleanAddress(
                 "6000 Kecskem\u00E9t Tat\u00E1r k\u00F6z 22. 1. emelet 1"),
               "6000 KECSKEMET TATAR 22")
  })
