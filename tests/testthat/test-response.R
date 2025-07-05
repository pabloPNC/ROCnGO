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

test_that("tranform_response works with fct and .condition is selected", {
  test_iris <- create_iris_df()
  expected_species <- test_iris$Species_bin_fct_virg
  transformed_species <- transform_response(test_iris$Species, "virginica")
  expect_equal(transformed_species, expected_species)
})


test_that("tranform_response works with int and .condition is selected", {
  test_iris <- create_iris_df()
  expected_species <- test_iris$Species_bin_fct_virg
  transformed_species <- transform_response(test_iris$Species_int, 3)
  expect_equal(transformed_species, expected_species)
})


test_that("tranform_response works with chr and .condition is selected", {
  test_iris <- create_iris_df()
  expected_species <- test_iris$Species_bin_fct_virg
  transformed_species <- transform_response(test_iris$Species_chr, "virginica")
  expect_equal(transformed_species, expected_species)
})

test_that("as_response works with fct(0,1) and .condition is selected", {
  test_iris <- create_iris_df()
  expected_species <- test_iris$Species_bin_fct_inv
  transformed_species <- as_response(
    x = test_iris$Species_bin_fct,
    .condition = "0"
  )
  expect_equal(transformed_species, expected_species)
})
