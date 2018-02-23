library(data.cleaning.HU)
context("cleanNames")

test_that("cégek megtisztulnak", {
    expect_equal(cleanNames(toupper(unAccent(
               c("Andego Tanácsadó Korlátolt felelősségű társaság",
                 "Magyar Telekom Nyrt")))),
               c("ANDEGO TANACSADO KFT", "MAGYAR TELEKOM RT"))
})

test_that("magánszemélyek megtisztulnak", {
  expect_identical(cleanNames("Sándor József"),
                   "SANDOR JOZSEF")
  expect_identical(cleanNames("Sándor József Benedek"),
                   "SANDOR JOZSEF")
  expect_identical(cleanNames("S. József Benedek"),
                   "S JOZSEF BENEDEK")
})