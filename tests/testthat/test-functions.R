test_that("load_data() only loads data for the vessel chosen", {
  subset <- load_data("Akacia")
  expect_true(subset %>% count(shipname) %>% .[1,2] %>% .[[1]] == nrow(subset))
})

test_that("check that calculate_max_distance() always returns 2 consecutive rows",{
  data <- load_data("Akacia")
  distance <- calculate_max_distance(data)
  expect_true( distance %>% nrow() == 2 )
  expect_true( distance$ID[2] - distance$ID[1] == 1 )
})

