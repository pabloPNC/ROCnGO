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
