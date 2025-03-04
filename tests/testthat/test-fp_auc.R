data <- tibble::tibble(readRDS(test_path("fixtures", "roc_data.rds")))
response <- "disease"
predictor <- "ENSG00000000003.15"

test_that("fp_auc == FpaucHS", {
  lower_threshold <- 1

  actual_fpauc <- suppressWarnings(
    fp_auc(
      data = NULL,
      response = data[[response]],
      predictor = data[[predictor]],
      lower_tpr = 0.9
    )
  )

  expected_fpauc <- FpaucHS(
    xsample = data[[response]],
    ysample = data[[predictor]],
    lower.sen = 0.9
  )

  expect_equal(
    actual_fpauc,
    expected_fpauc
  )
})

test_that("fp_auc works with .conditions", {
  test_iris <- create_iris_df()

  fpauc_fct <- suppressWarnings(
    fp_auc(
      test_iris,
      response = Species,
      predictor = Sepal.Length,
      lower_tpr = 0.9,
      .condition = "virginica"
    )
  )
  fpauc_int <- suppressWarnings(
    fp_auc(
      test_iris,
      response = Species_int,
      predictor = Sepal.Length,
      lower_tpr = 0.9,
      .condition = 3
    )
  )
  fpauc_chr <- suppressWarnings(
    fp_auc(
      test_iris,
      response = Species_chr,
      predictor = Sepal.Length,
      lower_tpr = 0.9,
      .condition = "virginica"
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
