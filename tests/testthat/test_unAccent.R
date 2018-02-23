library(data.cleaning.HU)
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

test_that("Result remains the same length in standard mode", {
  expect_equal(length(unAccent("á")),
               1)
  expect_equal(length(unAccent(c("á", "b"))),
               2)
  expect_equal(length(unAccent(c("á", "b", "Ő"))),
               3)
})

test_that("Result remains the same length in alternative mode", {
  expect_equal(length(unAccent("á", TRUE)),
               1)
  expect_equal(length(unAccent(c("á", "b"), TRUE)),
               2)
  expect_equal(length(unAccent(c("á", "b", "Ő"), TRUE)),
               3)
})

test_that("NA and others converts to character as expected", {
  expect_equal(unAccent(NA), NA_character_)
  expect_identical(unAccent(c("á", NA)),
                   c("a", NA_character_))
  expect_equal(unAccent(NA, TRUE),
               NA_character_)
  expect_identical(unAccent(c("á", NA), TRUE),
                   c("a", NA_character_))

  expect_equal(unAccent(factor(NA)), NA_character_)
  expect_identical(unAccent(factor(c("á", NA))),
                   c("a", NA_character_))
  expect_equal(unAccent(factor(NA, TRUE)), NA_character_)
  expect_identical(unAccent(factor(c("á", NA)), TRUE),
                   c("a", NA_character_))
  })
