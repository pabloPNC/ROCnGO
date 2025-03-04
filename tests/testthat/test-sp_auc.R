test_that("sp_auc works with .condition", {
  test_iris <- create_iris_df()
  spauc_fct <- suppressWarnings(
    sp_auc(
      test_iris,
      response = Species,
      predictor = Sepal.Length,
      lower_fpr = 0,
      upper_fpr = 0.3,
      .condition = "virginica"
    )
  )
  spauc_int <- suppressWarnings(
    sp_auc(
      test_iris,
      response = Species_int,
      predictor = Sepal.Length,
      lower_fpr = 0,
      upper_fpr = 0.3,
      .condition = 3
    )
  )
  spauc_chr <- suppressWarnings(
    sp_auc(
      test_iris,
      response = Species_chr,
      predictor = Sepal.Length,
      lower_fpr = 0,
      upper_fpr = 0.3,
      .condition = "virginica"
    )
  )
  expected_tpauc <- suppressWarnings(
    sp_auc(
      test_iris,
      response = Species_bin_fct_virg,
      predictor = Sepal.Length,
      lower_fpr = 0,
      upper_fpr = 0.3
    )
  )
  expect_equal(spauc_fct, expected_tpauc)
  expect_equal(spauc_int, expected_tpauc)
  expect_equal(spauc_chr, expected_tpauc)
})