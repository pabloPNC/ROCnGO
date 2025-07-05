test_that("tp_auc is correct", {
  test_iris <- create_iris_df()
  tpauc <- suppressMessages(
    suppressWarnings(
      tp_auc(
        data = test_iris,
        response = Species_bin_fct,
        predictor = Sepal.Width,
        lower_fpr = 0,
        upper_fpr = 0.5
      )
    )
  )
  expected_tpauc <- TpAUC.function(
    test_iris[["Species_bin_fct"]],
    test_iris[["Sepal.Width"]],
    lower.fp = 0,
    upper.fp = 0.5
  )
  expect_equal(tpauc, expected_tpauc)
})

test_that("tp_auc works with .condition", {
  test_iris <- create_iris_df()
  tpauc_fct <- suppressMessages(
    suppressWarnings(
      tp_auc(
        test_iris,
        response = Species,
        predictor = Sepal.Length,
        lower_fpr = 0,
        upper_fpr = 0.3,
        .condition = "virginica"
      )
    )
  )
  tpauc_int <- suppressMessages(
    suppressWarnings(
      tp_auc(
        test_iris,
        response = Species_int,
        predictor = Sepal.Length,
        lower_fpr = 0,
        upper_fpr = 0.3,
        .condition = 3
      )
    )
  )
  tpauc_chr <- suppressMessages(
    suppressWarnings(
      tp_auc(
        test_iris,
        response = Species_chr,
        predictor = Sepal.Length,
        lower_fpr = 0,
        upper_fpr = 0.3,
        .condition = "virginica"
      )
    )
  )
  expected_tpauc <- suppressMessages(
    suppressWarnings(
      tp_auc(
        test_iris,
        response = Species_bin_fct_virg,
        predictor = Sepal.Length,
        lower_fpr = 0,
        upper_fpr = 0.3
      )
    )
  )
  expect_equal(tpauc_fct, expected_tpauc)
  expect_equal(tpauc_int, expected_tpauc)
  expect_equal(tpauc_chr, expected_tpauc)
})
