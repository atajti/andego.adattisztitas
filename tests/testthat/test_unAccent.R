library(andego.adattisztitas)
context("unAccent")

test_that("unAccent works for HUN letters in standard mode", {
  expect_equal(unAccent("á", TRUE),
               "a")
  expect_equal(unAccent("Á", TRUE),
               "A")
  expect_equal(unAccent("é", TRUE),
               "e")
  expect_equal(unAccent("É", TRUE),
               "E")
  expect_equal(unAccent("Ó", TRUE),
               "O")
  expect_equal(unAccent("Ö", TRUE),
               "O")
  expect_equal(unAccent("Ő", TRUE),
               "O")
  expect_equal(unAccent("ó", TRUE),
               "o")
  expect_equal(unAccent("ö", TRUE),
               "o")
  expect_equal(unAccent("ő", TRUE),
               "o")
  expect_equal(unAccent("ú", TRUE),
               "u")
  expect_equal(unAccent("ü", TRUE),
               "u")
  expect_equal(unAccent("ű", TRUE),
               "u")
  expect_equal(unAccent("Ú", TRUE),
               "U")
  expect_equal(unAccent("Ü", TRUE),
               "U")
  expect_equal(unAccent("Ű", TRUE),
               "U")
})

test_that("unAccent works for HUN letters in alternative mode", {
  expect_equal(unAccent("á", TRUE),
               "a")
  expect_equal(unAccent("Á", TRUE),
               "A")
  expect_equal(unAccent("é", TRUE),
               "e")
  expect_equal(unAccent("É", TRUE),
               "E")
  expect_equal(unAccent("Ó", TRUE),
               "O")
  expect_equal(unAccent("Ö", TRUE),
               "O")
  expect_equal(unAccent("Ő", TRUE),
               "O")
  expect_equal(unAccent("ó", TRUE),
               "o")
  expect_equal(unAccent("ö", TRUE),
               "o")
  expect_equal(unAccent("ő", TRUE),
               "o")
  expect_equal(unAccent("ú", TRUE),
               "u")
  expect_equal(unAccent("ü", TRUE),
               "u")
  expect_equal(unAccent("ű", TRUE),
               "u")
  expect_equal(unAccent("Ú", TRUE),
               "U")
  expect_equal(unAccent("Ü", TRUE),
               "U")
  expect_equal(unAccent("Ű", TRUE),
               "U")
})
