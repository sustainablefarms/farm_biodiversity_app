
test_that("WCF buffers can be fetched using latitude and longitude", {
  wcfs <- canopyfromlatlon(145, -35, 2018)
  expect_equal(wcfs$`500m`, 0.72, tol = 0.01)
  expect_equal(wcfs$`3000m`, 1.04, tol = 0.01)
})
