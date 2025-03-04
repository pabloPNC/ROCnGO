test_that("summarize_predictor works with factor response", {
  test_df <- create_iris_df()
  expect_equal_nw(
    summarize_predictor(
      data = test_df,
      predictor = Sepal.Length,
      response = Species,
      ratio = "tpr",
      threshold = 0.9
    ),
    summarize_predictor(
      data = test_df,
      predictor = Sepal.Length,
      response = Species_bin_fct,
      ratio = "tpr",
      threshold = 0.9
    )
  )
})

test_that("summarize_predictor works with integer response", {
  test_df <- create_iris_df()
  expect_equal_nw(
    summarize_predictor(
      data = test_df,
      predictor = Sepal.Length,
      response = Species_int,
      ratio = "tpr",
      threshold = 0.9
    ),
    summarize_predictor(
      data = test_df,
      predictor = Sepal.Length,
      response = Species_bin_fct,
      ratio = "tpr",
      threshold = 0.9
    )
  )
})

test_that("summarize_predictor works with character response", {
  test_df <- create_iris_df()
  expect_equal_nw(
    summarize_predictor(
      data = test_df,
      predictor = Sepal.Length,
      response = Species_chr,
      ratio = "tpr",
      threshold = 0.9
    ),
    summarize_predictor(
      data = test_df,
      predictor = Sepal.Length,
      response = Species_bin_fct,
      ratio = "tpr",
      threshold = 0.9
    )
  )
})

test_that("summarize_dataset works with fct response", {
  test_df <- create_iris_df()
  predictors <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  expect_equal_nw(
    summarize_dataset(
      data = test_df,
      predictors = predictors,
      response = Species,
      ratio = "tpr",
      threshold = 0.9
    ),
    summarize_dataset(
      data = test_df,
      predictor = predictors,
      response = Species_bin_fct,
      ratio = "tpr",
      threshold = 0.9
    )
  )
})

test_that("summarize_dataset works with int response", {
  test_df <- create_iris_df()
  predictors <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  expect_equal_nw(
    summarize_dataset(
      data = test_df,
      predictors = predictors,
      response = Species_int,
      ratio = "tpr",
      threshold = 0.9
    ),
    summarize_dataset(
      data = test_df,
      predictor = predictors,
      response = Species_bin_fct,
      ratio = "tpr",
      threshold = 0.9
    )
  )
})

test_that("summarize_dataset works with chr response", {
  test_df <- create_iris_df()
  predictors <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  expect_equal_nw(
    summarize_dataset(
      data = test_df,
      predictors = predictors,
      response = Species_chr,
      ratio = "tpr",
      threshold = 0.9
    ),
    summarize_dataset(
      data = test_df,
      predictor = predictors,
      response = Species_bin_fct,
      ratio = "tpr",
      threshold = 0.9
    )
  )
})

test_that("summarize_predictor works with .condition", {
  test_iris <- create_iris_df()
  summarize_fct <- suppressWarnings(
    summarize_predictor(
      test_iris,
      predictor = Sepal.Length,
      response = Species,
      ratio = "tpr",
      threshold = 0.9,
      .condition = "virginica"
    )
  )
  summarize_int <- suppressWarnings(
    summarize_predictor(
      test_iris,
      predictor = Sepal.Length,
      response = Species_int,
      ratio = "tpr",
      threshold = 0.9,
      .condition = 3
    )
  )
  summarize_chr <- suppressWarnings(
    summarize_predictor(
      test_iris,
      predictor = Sepal.Length,
      response = Species_chr,
      ratio = "tpr",
      threshold = 0.9,
      .condition = "virginica"
    )
  )
  expected_summarize <- suppressWarnings(
    summarize_predictor(
      test_iris,
      predictor = Sepal.Length,
      response = Species_bin_fct_virg,
      ratio = "tpr",
      threshold = 0.9
    )
  )
  expect_equal(summarize_fct, expected_summarize)
  expect_equal(summarize_int, expected_summarize)
  expect_equal(summarize_chr, expected_summarize)
})


test_that("summarize_dataset works with .condition", {
  test_iris <- create_iris_df()
  predictors <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  summarize_fct <- suppressWarnings(
    summarize_dataset(
      test_iris,
      response = Species,
      predictors = predictors,
      ratio = "tpr",
      threshold = 0.9,
      .condition = "virginica"
    )
  )
  summarize_int <- suppressWarnings(
    summarize_dataset(
      test_iris,
      response = Species_int,
      predictors = predictors,
      ratio = "tpr",
      threshold = 0.9,
      .condition = 3
    )
  )
  summarize_chr <- suppressWarnings(
    summarize_dataset(
      test_iris,
      response = Species_chr,
      predictors = predictors,
      ratio = "tpr",
      threshold = 0.9,
      .condition = "virginica"
    )
  )
  expected_summarize <- suppressWarnings(
    summarize_dataset(
      test_iris,
      response = Species_bin_fct_virg,
      predictors = predictors,
      ratio = "tpr",
      threshold = 0.9
    )
  )
  expect_equal(summarize_fct, expected_summarize)
  expect_equal(summarize_int, expected_summarize)
  expect_equal(summarize_chr, expected_summarize)
})