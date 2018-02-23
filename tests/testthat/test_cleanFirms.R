library(data.cleaning.HU)
context("cégnevek tisztítása")

test_that("példa működik", {
  expect_equal(cleanFirms(toupper(unAccent(
               c("Andego Tanácsadó Korlátolt felelősségű társaság",
                 "Magyar Telekom Nyrt")))),
               c("ANDEGO TANACSADO KFT", "MAGYAR TELEKOM RT"))
  })

test_that("nem nagybetős-ékezettelített neveken működik",
  expect_equal(cleanFirms("GVC George's Venture Capital Zártkörűen Működő Részvénytársaság"),
               "GVC GEORGE'S VENTURE CAPITAL RT"))

test_that("&amp; kicserélődik",
  expect_equal(cleanFirms("K&amp;H ZRT"),
               "K&H RT"))
