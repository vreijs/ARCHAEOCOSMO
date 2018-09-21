context("Misc tests")

test_that("TopoArcvisionisSE", {
  expect_equal(
    S_TopoArcVisionisSE(
      -3,
      25,
      1,
      5,
      130,-45,
      0,
      1234567.5,
      123,
      50,
      10,
      15,
      1013.25,
      25,
      .3,
      1
    ),
    9.991035
  , tolerance = .0000001)
})

#Testresults for Declination-Altitude code
test_that("xxx", {
  expect_equal(TopoAltfromAppAlt(0, 15, 1013.25), -0.5598886, tolerance = .0000001)
})
test_that("Txxx", {
  expect_equal(AppAltfromTopoAlt(0, 15, 1013.25), 0.4721439, tolerance = .0000001)
})
test_that("Txxx", {
  expect_equal(GeoAltfromTopoAlt(-0.559888644263346, "moonavg"),
               0.3922204, tolerance = .0000001)
})
test_that("Txxx", {
  expect_equal(TopoAltfromGeoAlt(0, "moonavg"), -0.952, tolerance = .0000001)
})
test_that("Txxx", {
  expect_equal(ParallaxfromTopoAlt(-0.559888644263346, "moonavg"),
               0.9521091, tolerance = .0000001)
})
test_that("Txxx", {
  expect_equal(ParallaxfromGeoAlt(0, "moonavg"), 0.952, tolerance = .0000001)
})
test_that("Txxx", {
  expect_equal(S_Maxpar("moonavg"), 0.952, tolerance = .0000001)
})
test_that("Txxx", {
  expect_equal(GeoDecfromAppAlt(53, 0, 66, 0, "moonavg", 15, 1013.25),
               14.49181, tolerance = .000001)
})
test_that("Txxx", {
  expect_equal(GeoDecfromGeoAlt(53, 0, 66, 0), 14.16885, tolerance = .000001)
})
test_that("Txxx", {
  expect_equal(TopoAltfromDip(3000, 5) , -1.755515, tolerance = .0000001)
})
test_that("Txxx", {
  expect_equal(AppAltfromDip(3000, 5, 15, 1013.25, 0.0065), -1.589868, tolerance = .000001)
})
test_that("Txxx", {
  expect_equal(AppAltfromHeights(5, 3000, 2000, 15, 1013.25, 0.0065),
               56.25335, tolerance = .0000001)
})
test_that("Txxx", {
  expect_equal(TopoAltfromHeights(5, 3000, 2000), 56.25059, tolerance = .0000001)
})

test_that("GeoDecfromGeoAlt", {
  expect_equal(GeoDecfromGeoAlt(50,2,124,0), -19.419901780792, tolerance = .0000001)
})



test_that("GeoDecfromTopoAlt", {
  expect_equal(GeoDecfromAppAlt(50,2,124,0,"star"), -19.6664938960708, tolerance = .0000001)
})


test_that("hor2eq GeoAlt", {
  result<-astrolibR::hor2eq(2,124,2451545,50,0, refract_=F,nutate_=F,aberration_=F)
  expect_equal(result$dec, -19.419901780792, tolerance = .0000001)
})


test_that("hor2eq AppAlt", {
  result <- astrolibR::hor2eq(2,124,2451545,50,0, nutate_=F,aberration_=F)
  expect_equal(result$dec, -19.6689779937584, tolerance = .0000001)
})

