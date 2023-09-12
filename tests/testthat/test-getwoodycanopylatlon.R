
test_that("WCF buffers can be fetched using latitude and longitude", {
  wcfs <- canopyfromlatlon(145, -35, 2018)
  expect_equal(wcfs$`500m`, 0.72, tol = 0.01)
  expect_equal(wcfs$`3000m`, 1.04, tol = 0.01)
})

test_that("cloudget works", {
  point <- sf::st_point(x = c(145, -35), dim = "XY")
  pointwcrs <- sf::st_sf(sf::st_sfc(point, crs = 4326))
  
  within500m <- cloudget(pointwcrs, 500)
  expect_equal(within500m %>% extractayear(2018) %>% as.numeric(), 0.72, tol = 0.01)
})

test_that("cloudget fails nicely on flipped coords mistake", {
  point <- sf::st_point(x = c(-35, 145), dim = "XY")
  pointwcrs <- sf::st_sf(sf::st_sfc(point, crs = 4326))
  
  expect_error(within500m <- cloudget(pointwcrs, 500), "Invalid")
})

test_that("cloudget and threddgets get same result for 2020", {
  point <- sf::st_point(x = c(146.3333, -34.3333), dim = "XY")
  pointwcrs <- sf::st_sf(sf::st_sfc(point, crs = 4326))
  
  tget <- threddsget(pointwcrs, 3000, 1990:2021) %>%  #convert to same format as cloudget
    tibble::as_tibble(rownames = "Year") %>%
    tidyr::pivot_longer(everything(), 
                        names_to = "Year", values_to = "WCF",
                        names_prefix = "X"
    ) %>%
    dplyr::mutate(Year = as.integer(Year))
  
  cget <- cloudget(pointwcrs, 3000)
  expect_equal(tget, cget)
})

test_that("WCF buffers for 2020 are not strangely large", {
  point <- sf::st_point(x = c(146.3333, -34.3333), dim = "XY")
  pointwcrs <- sf::st_sf(sf::st_sfc(point, crs = 4326))
  
  within500m <- cloudget(pointwcrs, 500)
  within500m %>% ggplot() + geom_point(aes(x = Year, y = WCF))
  within3000m <- cloudget(pointwcrs, 3000)
  within3000m %>% ggplot() + geom_point(aes(x = Year, y = WCF))
  within500m_baseline <- mean(within500m$WCF[within500m$Year != 2020])
  within500m_baseline_sd <- sd(within500m$WCF[within500m$Year != 2020])
  expect_lt(abs(within500m$WCF[within500m$Year == 2020] - within500m_baseline), 2 * within500m_baseline_sd)
  
  within3000m_baseline <- mean(within3000m$WCF[within3000m$Year != 2020])
  within3000m_baseline_sd <- sd(within3000m$WCF[within3000m$Year != 2020])
  expect_lt(abs(within3000m$WCF[within3000m$Year == 2020] - within3000m_baseline), 2 * within3000m_baseline_sd)
})

test_that("WCF buffers for 2020 are strangely large from THREDDS", {
  point <- sf::st_point(x = c(146.3333, -34.3333), dim = "XY")
  pointwcrs <- sf::st_sf(sf::st_sfc(point, crs = 4326))
  
  within3000m <- threddsget(pointwcrs, 3000, 1990:2020) %>%  #convert to same format as cloudget
    tibble::as_tibble(rownames = "Year") %>%
    tidyr::pivot_longer(everything(), 
                        names_to = "Year", values_to = "WCF",
                        names_prefix = "X"
    ) %>%
    dplyr::mutate(Year = as.integer(Year))
  within3000m_baseline <- mean(within3000m$WCF[within3000m$Year != 2020])
  within3000m_baseline_sd <- sd(within3000m$WCF[within3000m$Year != 2020])
  expect_gt(abs(within3000m$WCF[within3000m$Year == 2020] - within3000m_baseline), 2 * within3000m_baseline_sd)
  })