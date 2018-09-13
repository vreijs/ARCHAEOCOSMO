context("Tests around periodicy")

test_that("Txxx", {
  expect_equal(SolarDayOpt(c(JDutfromDate(2000))), 24.00000017)
})
test_that("Txxx", {
  expect_equal(Sunobliquity(c(170686)), 24.12502276)
})
test_that("Txxx", {
  expect_equal(JDutfromDate("3.0.0.0.0"), 1016283)
})
test_that("Txxx", {
  expect_equal(JDutfromDate("35/12/1"), 1734175.5)
})
