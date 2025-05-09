test_that("get_thresholds is correct", {
  test_iris <- create_iris_df()
  thresholds <- get_thresholds(
    data = test_iris,
    predictor = Sepal.Width
  )
  expected_thresholds <- points.thresholds(
    test_iris[["Species_bin_fct"]],
    test_iris[["Sepal.Width"]]
  )
  expect_equal(thresholds, expected_thresholds)
})

test_that("calc_ratios, calc_fpr and calc_tpr are correct", {
  test_iris <- create_iris_df()
  thresholds <- get_thresholds(test_iris, predictor = Sepal.Width)
  tpr <- calc_tpr(
    data = test_iris,
    thresholds = thresholds,
    response = Species_bin_fct,
    predictor = Sepal.Width
  )
  fpr <- calc_fpr(
    data = test_iris,
    thresholds = thresholds,
    response = Species_bin_fct,
    predictor = Sepal.Width
  )
  ratios <- calc_ratios(
    data = test_iris,
    thresholds = thresholds,
    response = Species_bin_fct,
    predictor = Sepal.Width
  )
  sorted_tpr <- rev(tpr)
  sorted_fpr <- rev(fpr)
  sorted_tpr_ratio <- rev(ratios[["tpr"]])
  sorted_fpr_ratio <- rev(ratios[["fpr"]])
  expected_points <- points.curve(
    test_iris[["Species_bin_fct"]],
    test_iris[["Sepal.Width"]]
  )
  expect_equal(sorted_fpr, expected_points[, 1])
  expect_equal(sorted_tpr, expected_points[, 2])
  expect_equal(sorted_fpr_ratio, expected_points[, 1])
  expect_equal(sorted_tpr_ratio, expected_points[, 2])
})

test_that("sorting not needed in calc_*", {
  test_iris <- create_iris_df()
  thresholds <- get_thresholds(
    data = test_iris,
    predictor = Sepal.Width
  )
  ratios <- as_tibble(
    calc_ratios(
      data = test_iris,
      thresholds = thresholds,
      predictor = Sepal.Width,
      response = Species_bin_fct
    )
  )
  expected_ratios <- tibble(
    fpr = points.curve(
      test_iris[["Species_bin_fct"]],
      test_iris[["Sepal.Width"]]
    )[, 1],
    tpr = points.curve(
      test_iris[["Species_bin_fct"]],
      test_iris[["Sepal.Width"]]
    )[, 2]
  )
  expect_true(dplyr::setequal(ratios, expected_ratios))
})

test_that("roc_points works selecting a .condition", {
  test_iris <- create_iris_df()
  virg_fct_roc_points <- roc_points(
    test_iris,
    response = Species,
    predictor = Sepal.Length,
    .condition = "virginica"
  )
  virg_int_roc_points <- roc_points(
    test_iris,
    response = Species_int,
    predictor = Sepal.Length,
    .condition = 3
  )
  virg_chr_roc_points <- roc_points(
    test_iris,
    response = Species_chr,
    predictor = Sepal.Length,
    .condition = "virginica"
  )
  expected_roc_points <- roc_points(
    test_iris,
    response = Species_bin_fct_virg,
    predictor = Sepal.Length
  )
  expect_equal(virg_fct_roc_points, expected_roc_points)
  expect_equal(virg_int_roc_points, expected_roc_points)
  expect_equal(virg_chr_roc_points, expected_roc_points)
})
