# reads file in fixtures
data <- tibble::tibble(readRDS(test_path("fixtures", "roc_data.rds")))
response <- "disease"
predictor <- "ENSG00000000003.15"
# creates roc points of data/response | esto no sirve
tpr_fpr <- roc_points(
  response = data[[response]],
  predictor = data[[predictor]]
)
# cretaes roc points of data/response
partial_tpr_fpr <- calc_partial_roc_points(
  tpr = tpr_fpr[["tpr"]],
  fpr = tpr_fpr[["fpr"]],
  lower_threshold = 0.4,
  upper_threshold = 0.49,
  ratio = "fpr"
)

test_that("FPR diagonal_lower_bound is correct", {
  test_iris <- create_iris_df()
  partial_points <- suppressMessages(
    calc_partial_roc_points(
      data = test_iris,
      response = Species_bin_fct,
      predictor = Sepal.Width,
      lower_threshold = 0,
      upper_threshold = 0.5,
      ratio = "fpr"
    )
  )
  bound <- calc_fpr_diagonal_lower_bound(
    partial_fpr = partial_points[["partial_fpr"]],
    partial_tpr = partial_points[["partial_tpr"]]
  )
  expected_bound <- fpr.lower.bounds(
    test_iris[["Species_bin_fct"]],
    test_iris[["Sepal.Width"]],
    lower.fp = 0,
    upper.fp = 0.5
  )[["diagonal.pAUC"]]
  expect_equal(bound, expected_bound)
})


test_that("fpr diagonal_lower_bound are equal", {
  actual_bound <- calc_fpr_diagonal_lower_bound(
    partial_fpr = partial_tpr_fpr[["partial_fpr"]],
    partial_tpr = partial_tpr_fpr[["partial_tpr"]]
  )

  expected_bound <- fpr.lower.bounds(
    data[[response]],
    data[[predictor]],
    lower.fp = 0.4,
    upper.fp = 0.49
  )

  expect_equal(actual_bound, expected_bound[["diagonal.pAUC"]])
})

test_that("fpr square_lower_bound are equal", {
  actual_bound <- calc_fpr_square_lower_bound(
    partial_fpr = partial_tpr_fpr[["partial_fpr"]],
    partial_tpr = partial_tpr_fpr[["partial_tpr"]]
  )

  expected_bound <- fpr.lower.bounds(
    data[[response]],
    data[[predictor]],
    lower.fp = 0.4,
    upper.fp = 0.49
  )

  expect_equal(actual_bound, expected_bound[["TpAUC.min.roc"]])
})

test_that("fpr proper_lower_bound are equal", {
  actual_bound <- calc_fpr_proper_lower_bound(
    partial_fpr = partial_tpr_fpr[["partial_fpr"]],
    partial_tpr = partial_tpr_fpr[["partial_tpr"]]
  )

  expected_bound <- fpr.lower.bounds(
    data[[response]],
    data[[predictor]],
    lower.fp = 0.4,
    upper.fp = 0.49
  )

  expect_equal(actual_bound, expected_bound[["TpAUC.min.proper"]])
})

test_that("fpr plr_lower_bound are equal", {
  actual_bound <- calc_fpr_plr_lower_bound(
    partial_fpr = partial_tpr_fpr[["partial_fpr"]],
    partial_tpr = partial_tpr_fpr[["partial_tpr"]]
  )

  expected_bound <- fpr.lower.bounds(
    data[[response]],
    data[[predictor]],
    lower.fp = 0.4,
    upper.fp = 0.49
  )

  expect_equal(actual_bound, expected_bound[["TpAUC.min.dplr"]])
})

test_that("fpr calc_lower_bound are equal", {
  actual_bound <- calc_fpr_lower_bound(
    partial_fpr = partial_tpr_fpr[["partial_fpr"]],
    partial_tpr = partial_tpr_fpr[["partial_tpr"]]
  )

  expected_bound <- fpr.bounds(
    data[[response]],
    data[[predictor]],
    lower.fp = 0.4,
    upper.fp = 0.49
  )[["tp_auc_min"]]

  expect_equal(actual_bound, expected_bound)
})

test_that("fpr calc_upper_bound are equal", {
  actual_bound <- calc_fpr_upper_bound(
    partial_fpr = partial_tpr_fpr[["partial_fpr"]],
    partial_tpr = partial_tpr_fpr[["partial_tpr"]]
  )

  expected_bound <- fpr.bounds(
    data[[response]],
    data[[predictor]],
    lower.fp = 0.4,
    upper.fp = 0.49
  )[["tp_auc_max"]]

  expect_equal(actual_bound, expected_bound)
})

test_that("fpr calc_fpr_bounds are equal", {
  actual_bounds <- calc_fpr_bounds(
    partial_fpr = partial_tpr_fpr[["partial_fpr"]],
    partial_tpr = partial_tpr_fpr[["partial_tpr"]]
  )

  expected_bounds <- fpr.bounds(
    data[[response]],
    data[[predictor]],
    lower.fp = 0.4,
    upper.fp = 0.49
  )

  expect_equal(
    actual_bounds[["upper_bound"]],
    expected_bounds[["tp_auc_max"]]
  )
  expect_equal(
    actual_bounds[["lower_bound"]],
    expected_bounds[["tp_auc_min"]]
  )
})

test_that("curve_shape works with .condition", {
  test_iris <- create_iris_df()
  expect_equal_nw(
    calc_curve_shape(
      test_iris,
      Species,
      Sepal.Length,
      0,
      0.5,
      "tpr",
      "virginica"
    ),
    calc_curve_shape(
      test_iris,
      Species_bin_fct_virg,
      Sepal.Length,
      0,
      0.5,
      "tpr"
    )
  )
})