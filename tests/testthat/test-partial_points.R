data <- tibble::tibble(readRDS(test_path("fixtures", "roc_data.rds")))
response <- "disease"
predictor <- "ENSG00000000003.15"
tpr_fpr <- roc_points(
    response = data[[response]],
    predictor = data[[predictor]]
)

test_that("calc_indexes == partial.points.index", {
    sorted_fpr <- rev(tpr_fpr$fpr)

    actual_indexes <- calc_indexes(
        sorted_fpr,
        lower_threshold = 0,
        upper_threshold = 0.1
    )

    expected_indexes <- partial.points.indexes(
        data[[response]],
        data[[predictor]],
        lower.fp = 0,
        upper.fp = 0.1
    )

    expect_equal(actual_indexes[["lower"]], expected_indexes[["lower"]])
    expect_equal(actual_indexes[["upper"]], expected_indexes[["upper"]])
})

test_that("interp_lower_threshold warns when adding threshold - fpr", {
    sorted_fpr <- rev(tpr_fpr$fpr)
    sorted_tpr <- rev(tpr_fpr$tpr)

    indexes <- calc_indexes(
        sorted_fpr,
        lower_threshold = 0,
        upper_threshold = 0.1
    )

    expect_warning(
        interp_lower_threshold(
            ratio = sorted_fpr,
            interp_ratio = sorted_tpr,
            lower_threshold = 0,
            lower_index = indexes[["lower"]]
        )
    )
})

test_that("interp_upper_threshold warns when adding threshold - fpr", {
    sorted_fpr <- rev(tpr_fpr$fpr)
    sorted_tpr <- rev(tpr_fpr$tpr)

    indexes <- calc_indexes(
        sorted_fpr,
        lower_threshold = 0.9,
        upper_threshold = 1
    )

    expect_warning(
        interp_upper_threshold(
            ratio = sorted_fpr,
            interp_ratio = sorted_tpr,
            upper_threshold = 1,
            upper_index = indexes[["upper"]]
        )
    )
})

test_that("interp_lower_threshold == partial.points.curve[1] - fpr", {
    sorted_fpr <- rev(tpr_fpr$fpr)
    sorted_tpr <- rev(tpr_fpr$tpr)

    indexes <- calc_indexes(
        sorted_fpr,
        lower_threshold = 0.1,
        upper_threshold = 0.2
    )

    interp_ratios <- interp_lower_threshold(
        ratio = sorted_fpr,
        interp_ratio = sorted_tpr,
        lower_threshold = 0.1,
        lower_index = indexes[["lower"]]
    )

    expected_ratios <- partial.points.curve(
        data[[response]],
        data[[predictor]],
        lower.fp = 0.1,
        upper.fp = 0.2
    )

    expect_equal(
        interp_ratios[["interp_point"]],
        expected_ratios[["sen.pr"]][1]
    )
    expect_equal(
        interp_ratios[["threshold"]],
        expected_ratios[["fpr.pr"]][1]
    )
})

test_that("interp_upper_threshold == partial.points.curve[length] - fpr", {
    sorted_fpr <- rev(tpr_fpr$fpr)
    sorted_tpr <- rev(tpr_fpr$tpr)

    indexes <- calc_indexes(
        sorted_fpr,
        lower_threshold = 0.8,
        upper_threshold = 0.9
    )

    interp_ratios <- interp_upper_threshold(
        ratio = sorted_fpr,
        interp_ratio = sorted_tpr,
        upper_threshold = 0.9,
        upper_index = indexes[["upper"]]
    )

    expected_ratios <- partial.points.curve(
        data[[response]],
        data[[predictor]],
        lower.fp = 0.8,
        upper.fp = 0.9
    )

    last_index <- length(expected_ratios[["fpr.pr"]])

    expect_equal(
        interp_ratios[["interp_point"]],
        expected_ratios[["sen.pr"]][last_index]
    )
    expect_equal(
        interp_ratios[["threshold"]],
        expected_ratios[["fpr.pr"]][last_index]
    )
})

test_that("interp_thresholds == partial.points.curve[1] & [length]", {
    sorted_fpr <- rev(tpr_fpr$fpr)
    sorted_tpr <- rev(tpr_fpr$tpr)

    indexes <- calc_indexes(
        sorted_fpr,
        lower_threshold = 0.4,
        upper_threshold = 0.49
    )

    actual_ratios <- interp_thresholds(
        ratio = sorted_fpr,
        interp_ratio = sorted_tpr,
        lower_threshold = 0.4,
        upper_threshold = 0.49,
        lower_index = indexes[["lower"]],
        upper_index = indexes[["upper"]]
    )

    expected_ratios <- partial.points.curve(
        data[[response]],
        data[[predictor]],
        lower.fp = 0.4,
        upper.fp = 0.49
    )

    last_index <- length(expected_ratios[["fpr.pr"]])

    expect_equal(
        actual_ratios[["lower"]][["interp_point"]],
        expected_ratios[["sen.pr"]][1]
    )
    expect_equal(
        actual_ratios[["lower"]][["threshold"]],
        expected_ratios[["fpr.pr"]][1]
    )
    expect_equal(
        actual_ratios[["upper"]][["interp_point"]],
        expected_ratios[["sen.pr"]][last_index]
    )
    expect_equal(
        actual_ratios[["upper"]][["threshold"]],
        expected_ratios[["fpr.pr"]][last_index]
    )
})

test_that("calc_partial_roc_points == partial.points.curve - fpr", {
    sorted_fpr <- rev(tpr_fpr$fpr)
    sorted_tpr <- rev(tpr_fpr$tpr)

    actual_partial_points <- calc_partial_roc_points(
        tpr = sorted_tpr,
        fpr = sorted_fpr,
        lower_threshold = 0.4,
        upper_threshold = 0.49,
        ratio = "fpr"
    )

    expected_partial_points <- partial.points.curve(
        data[[response]],
        data[[predictor]],
        lower.fp = 0.4,
        upper.fp = 0.49
    )

    expect_equal(
        actual_partial_points[["partial_fpr"]],
        expected_partial_points[["fpr.pr"]]
    )
    expect_equal(
        actual_partial_points[["partial_tpr"]],
        expected_partial_points[["sen.pr"]]
    )
})

test_that("calc_partial_roc_points == pHSpoints - tpr", {
    sorted_fpr <- rev(tpr_fpr$fpr)
    sorted_tpr <- rev(tpr_fpr$tpr)

    actual_partial_points <- suppressWarnings(
        calc_partial_roc_points(
            tpr = sorted_tpr,
            fpr = sorted_fpr,
            lower_threshold = 0.9,
            upper_threshold = 1,
            ratio = "tpr"
        )
    )

    expected_partial_points <- pHSpoints(
        xsample = data[[response]],
        ysample = data[[predictor]],
        lower.sen = 0.9
    )

    expect_equal(
        actual_partial_points[["partial_fpr"]],
        expected_partial_points[,1]
    )
    expect_equal(
        actual_partial_points[["partial_tpr"]],
        expected_partial_points[,2]
    )
})
