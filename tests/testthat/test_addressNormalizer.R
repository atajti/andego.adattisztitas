library(andego.adattisztitas)
context("címtisztítás")

test_that("ékezetes városnevek", {
  expect_equal(cleanAddress(
                 "2011 Budakalász Kinizsi utca 33."),
               "2011 BUDAKALASZ KINIZSI 33")
  expect_equal(cleanAddress(
                 "2011 BUDAKALÁSZ Kinizsi utca 33."),
               "2011 BUDAKALASZ KINIZSI 33")
  expect_equal(cleanAddress(
                 "6000 Kecskemét Tatár sor 22"),
               "6000 KECSKEMET TATAR 22")
  expect_equal(cleanAddress(
                 "6000 KECSKEMÉT Tatár sor 22"),
                 "6000 KECSKEMET TATAR 22")
  })

test_that("közterületek", {
  expect_equal(cleanAddress(
                 "2011 Budakalász Kinizsi u. 33."),
               "2011 BUDAKALASZ KINIZSI 33")
  expect_equal(cleanAddress(                 
                 "2011 Budakalász Kinizsi utca 33."),
               "2011 BUDAKALASZ KINIZSI 33")
  expect_equal(cleanAddress(                 
                 "6000 Kecskemét Tatár sor 22"),
               "6000 KECSKEMET TATAR 22")
  expect_equal(cleanAddress(                 
                 "6000 Kecskemét Tatár tér 22"),
               "6000 KECSKEMET TATAR 22")
  expect_equal(cleanAddress(                 
                 "6000 Kecskemét Tatár út 22"),
               "6000 KECSKEMET TATAR 22")
  expect_equal(cleanAddress(                 
                 "6000 Kecskemét Tatár köz 22"),
               "6000 KECSKEMET TATAR 22")
  })

test_that("bonyolult lakáscímek", {
  expect_equal(cleanAddress(
                 "6000 Kecskemét Tatár köz 22"),
               "6000 KECSKEMET TATAR 22")
  expect_equal(cleanAddress(
                 "6000 Kecskemét Tatár köz 22/b"),
               "6000 KECSKEMET TATAR 22")
  expect_equal(cleanAddress(
                 "6000 Kecskemét Tatár köz 22-23"),
               "6000 KECSKEMET TATAR 22")
  expect_equal(cleanAddress(
                 "6000 Kecskemét Tatár köz 22. 1. em. 1."),
                 "6000 KECSKEMET TATAR 22")
  expect_equal(cleanAddress(
                 "6000 Kecskemét Tatár köz 22. 1. emelet 1"),
               "6000 KECSKEMET TATAR 22")
  })
