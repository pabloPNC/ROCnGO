data <- tibble::tibble(readRDS(test_path("fixtures", "roc_data.rds")))
response <- "disease"
predictor <- "ENSG00000000003.15"
tpr_fpr <- roc_points(
  response = data[[response]],
  predictor = data[[predictor]]
)

test_that("calc_indexes is correct", {
  test_iris <- create_iris_df()
  ratios <- roc_points(
    data = test_iris,
    response = Species_bin_fct,
    predictor = Sepal.Width
  )
  sorted_fpr <- rev(ratios$fpr)
  indexes <- calc_indexes(
    sorted_fpr,
    lower_threshold = 0,
    upper_threshold = 0.1
  )
  expected_indexes <- partial.points.indexes(
    test_iris[["Species_bin_fct"]],
    test_iris[["Sepal.Width"]],
    lower.fp = 0,
    upper.fp = 0.1
  )
  expect_equal(indexes[["lower"]], expected_indexes[["lower"]])
  expect_equal(indexes[["upper"]], expected_indexes[["upper"]])
})

test_that("interp_lower_threshold throws a message when not adding threshold", {
  test_iris <- create_iris_df()
  ratios <- roc_points(
    data = test_iris,
    response = Species_bin_fct,
    predictor = Sepal.Width
  )
  sorted_fpr <- rev(ratios$fpr)
  sorted_tpr <- rev(ratios$tpr)
  indexes <- calc_indexes(
    ratio = sorted_fpr,
    lower_threshold = 0,
    upper_threshold = 0.1
  )
  expect_message(
    expect_message(
      interp_lower_threshold(
        ratio = sorted_fpr,
        interp_ratio = sorted_tpr,
        lower_threshold = 0,
        lower_index = indexes[["lower"]]
      ),
      class = "inform_lower_threshold"
    ),
    class = "cliMessage"
  )
})

test_that("interp_upper_threshold throws a message when not adding threshold", {
  test_iris <- create_iris_df()
  ratios <- roc_points(
    data = test_iris,
    response = Species_bin_fct,
    predictor = Sepal.Width
  )
  sorted_fpr <- rev(ratios$fpr)
  sorted_tpr <- rev(ratios$tpr)
  indexes <- calc_indexes(
    ratio = sorted_fpr,
    lower_threshold = 0.9,
    upper_threshold = 1
  )
  expect_message(
    expect_message(
      interp_upper_threshold(
        ratio = sorted_fpr,
        interp_ratio = sorted_tpr,
        upper_threshold = 1,
        upper_index = indexes[["upper"]]
      ),
      class = "inform_upper_threshold"
    ),
    class = "cliMessage"
  )
})

test_that("interp_lower_threshold is correct", {
  test_iris <- create_iris_df()
  ratios <- roc_points(
    data = test_iris,
    response = Species_bin_fct,
    predictor = Sepal.Width
  )
  sorted_fpr <- rev(ratios$fpr)
  sorted_tpr <- rev(ratios$tpr)
  indexes <- calc_indexes(
    ratio = sorted_fpr,
    lower_threshold = 0.2,
    upper_threshold = 0.5
  )
  threshold_point <- interp_lower_threshold(
    ratio = sorted_fpr,
    interp_ratio = sorted_tpr,
    lower_threshold = 0.2,
    lower_index = indexes[["lower"]]
  )
  expected_ratios <- partial.points.curve(
    test_iris[["Species_bin_fct"]],
    test_iris[["Sepal.Width"]],
    lower.fp = 0.2,
    upper.fp = 0.5
  )
  expect_equal(
    threshold_point[["interp_point"]],
    expected_ratios[["sen.pr"]][1]
  )
  expect_equal(threshold_point[["threshold"]], expected_ratios[["fpr.pr"]][1])
})

test_that("interp_upper_threshold is correct", {
  test_iris <- create_iris_df()
  ratios <- roc_points(
    data = test_iris,
    response = Species_bin_fct,
    predictor = Sepal.Width
  )
  sorted_fpr <- rev(ratios$fpr)
  sorted_tpr <- rev(ratios$tpr)
  indexes <- calc_indexes(
    ratio = sorted_fpr,
    lower_threshold = 0.2,
    upper_threshold = 0.5
  )
  threshold_point <- interp_upper_threshold(
    ratio = sorted_fpr,
    interp_ratio = sorted_tpr,
    upper_threshold = 0.5,
    upper_index = indexes[["upper"]]
  )
  expected_ratios <- partial.points.curve(
    test_iris[["Species_bin_fct"]],
    test_iris[["Sepal.Width"]],
    lower.fp = 0.2,
    upper.fp = 0.5
  )
  last_index <- length(expected_ratios[["fpr.pr"]])
  expect_equal(
    threshold_point[["interp_point"]],
    expected_ratios[["sen.pr"]][last_index]
  )
  expect_equal(
    threshold_point[["threshold"]],
    expected_ratios[["fpr.pr"]][last_index]
  )
})

test_that("interp_thresholds is correct", {
  test_iris <- create_iris_df()
  ratios <- roc_points(
    data = test_iris,
    response = Species_bin_fct,
    predictor = Sepal.Width
  )
  sorted_fpr <- rev(ratios$fpr)
  sorted_tpr <- rev(ratios$tpr)
  indexes <- calc_indexes(
    ratio = sorted_fpr,
    lower_threshold = 0.2,
    upper_threshold = 0.5
  )
  interp_points <- interp_thresholds(
    ratio = sorted_fpr,
    interp_ratio = sorted_tpr,
    lower_threshold = 0.2,
    upper_threshold = 0.5,
    lower_index = indexes[["lower"]],
    upper_index = indexes[["upper"]]
  )
  expected_ratios <- partial.points.curve(
    test_iris[["Species_bin_fct"]],
    test_iris[["Sepal.Width"]],
    lower.fp = 0.2,
    upper.fp = 0.5
  )
  last_index <- length(expected_ratios[["fpr.pr"]])
  expect_equal(
    interp_points[["lower"]][["interp_point"]],
    expected_ratios[["sen.pr"]][1]
  )
  expect_equal(
    interp_points[["lower"]][["threshold"]],
    expected_ratios[["fpr.pr"]][1]
  )
  expect_equal(
    interp_points[["upper"]][["interp_point"]],
    expected_ratios[["sen.pr"]][last_index]
  )
  expect_equal(
    interp_points[["upper"]][["threshold"]],
    expected_ratios[["fpr.pr"]][last_index]
  )
})

test_that("FPR calc_partial_roc_points is correct", {
  test_iris <- create_iris_df()
  ppoints <- calc_partial_roc_points(
    data = test_iris,
    response = Species_bin_fct,
    predictor = Sepal.Width,
    lower_threshold = 0.2,
    upper_threshold = 0.5,
    ratio = "fpr"
  )
  expected_ppoints <- partial.points.curve(
    test_iris[["Species_bin_fct"]],
    test_iris[["Sepal.Width"]],
    lower.fp = 0.2,
    upper.fp = 0.5
  )
  expect_equal(
    ppoints[["partial_fpr"]],
    expected_ppoints[["fpr.pr"]]
  )
  expect_equal(
    ppoints[["partial_tpr"]],
    expected_ppoints[["sen.pr"]]
  )
})

test_that("TPR calc_partial_roc_points is correct", {
  test_iris <- create_iris_df()
  ppoints <- suppressMessages(
    calc_partial_roc_points(
      data = test_iris,
      response = Species_bin_fct,
      predictor = Sepal.Width,
      lower_threshold = 0.9,
      upper_threshold = 1,
      ratio = "tpr"
    )
  )
  expected_ppoints <- pHSpoints(
    test_iris[["Species_bin_fct"]],
    test_iris[["Sepal.Width"]],
    lower.sen = 0.9
  )
  expect_equal(
    ppoints[["partial_fpr"]],
    expected_ppoints[, 1]
  )
  expect_equal(
    ppoints[["partial_tpr"]],
    expected_ppoints[, 2]
  )
})

test_that("calc_partial_points works with .condition", {
  test_iris <- create_iris_df()

  partial_points_fct <- suppressWarnings(
    calc_partial_roc_points(
      test_iris,
      response = Species,
      predictor = Sepal.Length,
      lower_threshold = 0,
      upper_threshold = 0.9,
      ratio = "fpr",
      .condition = "virginica"
    )
  )
  partial_points_int <- suppressWarnings(
    calc_partial_roc_points(
      test_iris,
      response = Species_int,
      predictor = Sepal.Length,
      lower_threshold = 0,
      upper_threshold = 0.9,
      ratio = "fpr",
      .condition = 3
    )
  )
  partial_points_chr <- suppressWarnings(
    calc_partial_roc_points(
      test_iris,
      response = Species_chr,
      predictor = Sepal.Length,
      lower_threshold = 0,
      upper_threshold = 0.9,
      ratio = "fpr",
      .condition = "virginica"
    )
  )
  expected_partial_points <- suppressWarnings(
    calc_partial_roc_points(
      test_iris,
      response = Species_bin_fct_virg,
      predictor = Sepal.Length,
      lower_threshold = 0,
      upper_threshold = 0.9,
      ratio = "fpr"
    )
  )
  expect_equal(partial_points_fct, expected_partial_points)
  expect_equal(partial_points_int, expected_partial_points)
  expect_equal(partial_points_chr, expected_partial_points)
})
