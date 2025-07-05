test_that("FpAUC is correct", {
  test_iris <- create_iris_df()
  fpauc <- suppressMessages(
    suppressWarnings(
      fp_auc(
        data = test_iris,
        response = Species_bin_fct,
        predictor = Sepal.Width,
        lower_tpr = 0.9
      )
    )
  )
  expected_fpauc <- FpaucHS(
    xsample = test_iris[["Species_bin_fct"]],
    ysample = test_iris[["Sepal.Width"]],
    lower.sen = 0.9
  )
  expect_equal(fpauc, expected_fpauc)
})

test_that("fp_auc works with .conditions", {
  test_iris <- create_iris_df()

  fpauc_fct <- suppressMessages(
    suppressWarnings(
      fp_auc(
        test_iris,
        response = Species,
        predictor = Sepal.Length,
        lower_tpr = 0.9,
        .condition = "virginica"
      )
    )
  )
  fpauc_int <- suppressMessages(
    suppressWarnings(
      fp_auc(
        test_iris,
        response = Species_int,
        predictor = Sepal.Length,
        lower_tpr = 0.9,
        .condition = 3
      )
    )
  )
  fpauc_chr <- suppressMessages(
    suppressWarnings(
      fp_auc(
        test_iris,
        response = Species_chr,
        predictor = Sepal.Length,
        lower_tpr = 0.9,
        .condition = "virginica"
      )
    )
  )
  expected_fpauc <- FpaucHS(
    xsample = test_iris$Species_bin_fct_virg,
    ysample = test_iris$Sepal.Length,
    lower.sen = 0.9
  )
  expect_equal(fpauc_fct, expected_fpauc)
  expect_equal(fpauc_int, expected_fpauc)
  expect_equal(fpauc_chr, expected_fpauc)
})
