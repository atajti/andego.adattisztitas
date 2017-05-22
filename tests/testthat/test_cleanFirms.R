library(andego.adattisztitas)
context("cégnevek tisztítása")

test_that("példa működik", {
  expect_equal(cleanFirms(toupper(unAccent(
               c("Andego Tanácsadó Korlátolt felelősségű társaság",
                 "Magyar Telekom Nyrt")))),
               c("ANDEGO TANACSADO KFT", "MAGYAR TELEKOM RT"))
  })