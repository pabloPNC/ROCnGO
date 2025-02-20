test_that("transform_response works with factors", {
  expected_species <- iris %>%
    pull(Species) %>%
    fct_collapse("0" = c("virginica", "versicolor"), "1" = "setosa") %>%
    fct_relevel("0", "1")
  transformed_species <- transform_response(iris$Species)
  expect_equal(transformed_species, expected_species)
})

test_that("transform_response works with integers", {
  expected_species <- iris %>%
    pull(Species) %>%
    fct_collapse("0" = c("virginica", "versicolor"), "1" = "setosa") %>%
    fct_relevel("0", "1")
  transformed_species <- dplyr::case_when(
    iris$Species == "setosa" ~ 1L,
    iris$Species == "virginica" ~ 2L,
    iris$Species == "versicolor" ~ 3L
  ) %>% transform_response()
  expect_equal(transformed_species, expected_species)
})

test_that("transform_response works with characters", {
  expected_species <- iris %>%
    pull(Species) %>%
    fct_collapse("0" = c("virginica", "versicolor"), "1" = "setosa") %>%
    fct_relevel("0", "1")
  transformed_species <- transform_response(as.character(iris$Species))
  expect_equal(transformed_species, expected_species)
})