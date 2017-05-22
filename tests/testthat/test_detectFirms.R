library(andego.adattisztitas)
context("cégdetektálás")

test_that("példa működik", {
  expect_equal(detectFirms(toupper(unAccent(
               c("Andego Tanácsadó Korlátolt felelősségű társaság",
                 "Magyar Telekom Nyrt",
                 "Tajti András")))),
               c(TRUE, TRUE, FALSE))
  })