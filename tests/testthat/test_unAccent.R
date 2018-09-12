library(data.cleaning.HU)
context("unAccent")

test_that("unAccent works for HUN letters in standard mode", {
  expect_equal(unAccent("\u00E1", TRUE),
               "a")
  expect_equal(unAccent("\u00C1", TRUE),
               "A")
  expect_equal(unAccent("\u00E9", TRUE),
               "e")
  expect_equal(unAccent("\u00C9", TRUE),
               "E")
  expect_equal(unAccent("\u00D3", TRUE),
               "O")
  expect_equal(unAccent("\u00D6", TRUE),
               "O")
  expect_equal(unAccent("\u0150", TRUE),
               "O")
  expect_equal(unAccent("\u00F3", TRUE),
               "o")
  expect_equal(unAccent("\u00F6", TRUE),
               "o")
  expect_equal(unAccent("\u0151", TRUE),
               "o")
  expect_equal(unAccent("\u00FA", TRUE),
               "u")
  expect_equal(unAccent("\u00FC", TRUE),
               "u")
  expect_equal(unAccent("\u0171", TRUE),
               "u")
  expect_equal(unAccent("\u00DA", TRUE),
               "U")
  expect_equal(unAccent("\u00DC", TRUE),
               "U")
  expect_equal(unAccent("\u0170", TRUE),
               "U")
})

test_that("unAccent works for HUN letters in alternative mode", {
  expect_equal(unAccent("\u00E1", TRUE),
               "a")
  expect_equal(unAccent("\u00C1", TRUE),
               "A")
  expect_equal(unAccent("\u00E9", TRUE),
               "e")
  expect_equal(unAccent("\u00C9", TRUE),
               "E")
  expect_equal(unAccent("\u00D3", TRUE),
               "O")
  expect_equal(unAccent("\u00D6", TRUE),
               "O")
  expect_equal(unAccent("\u0150", TRUE),
               "O")
  expect_equal(unAccent("\u00F3", TRUE),
               "o")
  expect_equal(unAccent("\u00F6", TRUE),
               "o")
  expect_equal(unAccent("\u0151", TRUE),
               "o")
  expect_equal(unAccent("\u00FA", TRUE),
               "u")
  expect_equal(unAccent("\u00FC", TRUE),
               "u")
  expect_equal(unAccent("\u0171", TRUE),
               "u")
  expect_equal(unAccent("\u00DA", TRUE),
               "U")
  expect_equal(unAccent("\u00DC", TRUE),
               "U")
  expect_equal(unAccent("\u0170", TRUE),
               "U")
})

test_that("Result remains the same length in standard mode", {
  expect_equal(length(unAccent("\u00E1")),
               1)
  expect_equal(length(unAccent(c("\u00E1", "b"))),
               2)
  expect_equal(length(unAccent(c("\u00E1", "b", "\u0150"))),
               3)
})

test_that("Result remains the same length in alternative mode", {
  expect_equal(length(unAccent("\u00E1", TRUE)),
               1)
  expect_equal(length(unAccent(c("\u00E1", "b"), TRUE)),
               2)
  expect_equal(length(unAccent(c("\u00E1", "b", "\u0150"), TRUE)),
               3)
})

test_that("NA and others converts to character as expected", {
  expect_equal(unAccent(NA), NA_character_)
  expect_identical(unAccent(c("\u00E1", NA)),
                   c("a", NA_character_))
  expect_equal(unAccent(NA, TRUE),
               NA_character_)
  expect_identical(unAccent(c("\u00E1", NA), TRUE),
                   c("a", NA_character_))

  expect_equal(unAccent(factor(NA)), NA_character_)
  expect_identical(unAccent(factor(c("\u00E1", NA))),
                   c("a", NA_character_))
  expect_equal(unAccent(factor(NA, TRUE)), NA_character_)
  expect_identical(unAccent(factor(c("\u00E1", NA)), TRUE),
                   c("a", NA_character_))
  })
