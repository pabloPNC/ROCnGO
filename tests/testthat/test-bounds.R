test_that("FPR diagonal_lower_bound is correct", {
  test_iris <- create_iris_df()
  partial_points <- suppressMessages(
    suppressWarnings(
      calc_partial_roc_points(
        data = test_iris,
        response = Species_bin_fct,
        predictor = Sepal.Width,
        lower_threshold = 0,
        upper_threshold = 0.5,
        ratio = "fpr"
      )
    )
  )
  bound <- calc_fpr_diagonal_lower_bound(
    partial_fpr = partial_points[["fpr"]],
    partial_tpr = partial_points[["tpr"]]
  )
  expected_bound <- fpr.lower.bounds(
    test_iris[["Species_bin_fct"]],
    test_iris[["Sepal.Width"]],
    lower.fp = 0,
    upper.fp = 0.5
  )[["diagonal.pAUC"]]
  expect_equal(bound, expected_bound)
})

test_that("FPR rectangle lower bound is correct", {
  test_iris <- create_iris_df()
  partial_points <- suppressMessages(
    suppressWarnings(
      calc_partial_roc_points(
        data = test_iris,
        response = Species_bin_fct,
        predictor = Sepal.Width,
        lower_threshold = 0,
        upper_threshold = 0.5,
        ratio = "fpr"
      )
    )
  )
  bound <- calc_fpr_square_lower_bound(
    partial_fpr = partial_points[["fpr"]],
    partial_tpr = partial_points[["tpr"]]
  )
  expected_bound <- fpr.lower.bounds(
    test_iris[["Species_bin_fct"]],
    test_iris[["Sepal.Width"]],
    lower.fp = 0,
    upper.fp = 0.5
  )[["TpAUC.min.roc"]]
  expect_equal(bound, expected_bound)
})

test_that("FPR proper lower bound is correct", {
  test_iris <- create_iris_df()
  partial_points <- suppressMessages(
    suppressWarnings(
      calc_partial_roc_points(
        data = test_iris,
        response = Species_bin_fct,
        predictor = Sepal.Width,
        lower_threshold = 0,
        upper_threshold = 0.5,
        ratio = "fpr"
      )
    )
  )
  bound <- calc_fpr_proper_lower_bound(
    partial_fpr = partial_points[["fpr"]],
    partial_tpr = partial_points[["tpr"]]
  )
  expected_bound <- fpr.lower.bounds(
    test_iris[["Species_bin_fct"]],
    test_iris[["Sepal.Width"]],
    lower.fp = 0,
    upper.fp = 0.5
  )[["TpAUC.min.proper"]]
  expect_equal(bound, expected_bound)
})

test_that("FPR PLR/Concave lower bound is correct", {
  test_iris <- create_iris_df()
  partial_points <- suppressMessages(
    suppressWarnings(
      calc_partial_roc_points(
        data = test_iris,
        response = Species_bin_fct,
        predictor = Sepal.Width,
        lower_threshold = 0,
        upper_threshold = 0.5,
        ratio = "fpr"
      )
    )
  )
  bound <- calc_fpr_plr_lower_bound(
    partial_fpr = partial_points[["fpr"]],
    partial_tpr = partial_points[["tpr"]]
  )
  expected_bound <- fpr.lower.bounds(
    test_iris[["Species_bin_fct"]],
    test_iris[["Sepal.Width"]],
    lower.fp = 0,
    upper.fp = 0.5
  )[["TpAUC.min.dplr"]]
  expect_equal(bound, expected_bound)
})

test_that("FPR lower bound is correctly selected", {
  test_iris <- create_iris_df()
  partial_points <- suppressMessages(
    suppressWarnings(
      calc_partial_roc_points(
        data = test_iris,
        response = Species_bin_fct,
        predictor = Sepal.Width,
        lower_threshold = 0,
        upper_threshold = 0.5,
        ratio = "fpr"
      )
    )
  )
  bound <- calc_fpr_lower_bound(
    partial_fpr = partial_points[["fpr"]],
    partial_tpr = partial_points[["tpr"]]
  )
  expected_bound <- fpr.bounds(
    test_iris[["Species_bin_fct"]],
    test_iris[["Sepal.Width"]],
    lower.fp = 0,
    upper.fp = 0.5
  )[["tp_auc_min"]]
  expect_equal(bound, expected_bound)
})

test_that("FPR upper bound is correct", {
  test_iris <- create_iris_df()
  partial_points <- suppressMessages(
    suppressWarnings(
      calc_partial_roc_points(
        data = test_iris,
        response = Species_bin_fct,
        predictor = Sepal.Width,
        lower_threshold = 0,
        upper_threshold = 0.5,
        ratio = "fpr"
      )
    )
  )
  bound <- calc_fpr_upper_bound(
    partial_fpr = partial_points[["fpr"]],
    partial_tpr = partial_points[["tpr"]]
  )
  expected_bound <- fpr.bounds(
    test_iris[["Species_bin_fct"]],
    test_iris[["Sepal.Width"]],
    lower.fp = 0,
    upper.fp = 0.5
  )[["tp_auc_max"]]
  expect_equal(bound, expected_bound)
})

test_that("FPR both bounds are correct", {
  test_iris <- create_iris_df()
  partial_points <- suppressMessages(
    suppressWarnings(
      calc_partial_roc_points(
        data = test_iris,
        response = Species_bin_fct,
        predictor = Sepal.Width,
        lower_threshold = 0,
        upper_threshold = 0.5,
        ratio = "fpr"
      )
    )
  )
  bounds <- calc_fpr_bounds(
    partial_fpr = partial_points[["fpr"]],
    partial_tpr = partial_points[["tpr"]]
  )
  expected_bounds <- fpr.bounds(
    test_iris[["Species_bin_fct"]],
    test_iris[["Sepal.Width"]],
    lower.fp = 0,
    upper.fp = 0.5
  )
  expect_equal(bounds[["upper_bound"]], expected_bounds[["tp_auc_max"]])
  expect_equal(bounds[["lower_bound"]], expected_bounds[["tp_auc_min"]])
})

test_that("curve_shape works with .condition", {
  test_iris <- create_iris_df()
  curve_shape <- suppressMessages(
    suppressWarnings(
      calc_curve_shape(
        data = test_iris,
        response = Species,
        predictor = Sepal.Length,
        lower_threshold = 0,
        upper_threshold = 0.5,
        ratio = "tpr",
        .condition = "virginica"
      )
    )
  )
  expected_curve_shape <- suppressMessages(
    suppressWarnings(
      calc_curve_shape(
        data = test_iris,
        response = Species_bin_fct_virg,
        predictor = Sepal.Length,
        lower_threshold = 0,
        upper_threshold = 0.5,
        ratio = "tpr"
      )
    )
  )
  expect_equal(curve_shape, expected_curve_shape)
})
