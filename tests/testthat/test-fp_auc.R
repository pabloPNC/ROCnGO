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
