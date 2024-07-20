data <- tibble::tibble(readRDS(test_path("fixtures", "roc_data.rds")))
response <- "disease"
predictor <- "ENSG00000000003.15"
tpr_fpr <- roc_points(
    response = data[[response]],
    predictor = data[[predictor]]
)

partial_tpr_fpr <- calc_partial_roc_points(
    tpr = tpr_fpr[["tpr"]],
    fpr = tpr_fpr[["fpr"]],
    lower_threshold = 0.4,
    upper_threshold = 0.49,
    ratio = "fpr",
    sort = TRUE,
    include_thresholds = TRUE
)


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

    actual_bound <- calc_lower_bound(
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

    actual_bound <- calc_upper_bound(
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
