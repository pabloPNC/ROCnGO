test_that("np_auc works with .conditions", {
  test_iris <- create_iris_df()

  np_auc_fct <- suppressWarnings(
    np_auc(
      test_iris,
      response = Species,
      predictor = Sepal.Length,
      .condition = "virginica",
      lower_tpr = 0.9
    )
  )
  np_auc_int <- suppressWarnings(
    np_auc(
      test_iris,
      response = Species_int,
      predictor = Sepal.Length,
      .condition = 3,
      lower_tpr = 0.9
    )
  )
  np_auc_chr <- suppressWarnings(
    np_auc(
      test_iris,
      response = Species_chr,
      predictor = Sepal.Length,
      .condition = "virginica",
      lower_tpr = 0.9
    )
  )
  expected_npauc <- suppressWarnings(
    np_auc(
      test_iris,
      response = Species_bin_fct_virg,
      predictor = Sepal.Length,
      lower_tpr = 0.9
    )
  )
  expect_equal(np_auc_fct, expected_npauc)
  expect_equal(np_auc_int, expected_npauc)
  expect_equal(np_auc_chr, expected_npauc)
})