data <- tibble::tibble(readRDS(test_path("fixtures", "roc_data.rds")))
response <- "disease"
predictor <- "ENSG00000000003.15"

test_that("get_thresholds == points.thresholds", {
  expect_equal(
    get_thresholds(predictor = data[[predictor]]),
    points.thresholds(data[[response]], data[[predictor]])
  )
})

test_that("get_thresholds == points.thresholds - tidy", {
  expect_equal(
    get_thresholds(data, ENSG00000000003.15),
    points.thresholds(data[[response]], data[[predictor]])
  )
})

test_that("calc_ratios, calc_fpr and calc_tpr == points.curve", {
  expect_equal(
    rev(
      calc_tpr(
        thresholds = get_thresholds(
          predictor = data[[predictor]],
        ),
        response = data[[response]],
        predictor = data[[predictor]]
      )
    ),
    points.curve(data[[response]], data[[predictor]])[, 2]
  )
  expect_equal(
    rev(
      calc_fpr(
        thresholds = get_thresholds(
          predictor = data[[predictor]],
        ),
        response = data[[response]],
        predictor = data[[predictor]]
      )
    ),
    points.curve(data[[response]], data[[predictor]])[, 1]
  )
  expect_equal(
    rev(
      calc_ratios(
        thresholds = get_thresholds(
          predictor = data[[predictor]],
        ),
        response = data[[response]],
        predictor = data[[predictor]]
      )[["fpr"]]
    ),
    points.curve(data[[response]], data[[predictor]])[, 1]
  )
  expect_equal(
    rev(
      calc_ratios(
        thresholds = get_thresholds(
          predictor = data[[predictor]],
        ),
        response = data[[response]],
        predictor = data[[predictor]]
      )[["tpr"]]
    ),
    points.curve(data[[response]], data[[predictor]])[, 2]
  )
})

test_that("calc_ratios, calc_fpr and calc_tpr == points.curve - tidy", {
  expect_equal(
    rev(
      calc_tpr(
        data,
        thresholds = get_thresholds(
          predictor = data[[predictor]],
        ),
        response = disease,
        predictor = ENSG00000000003.15
      )
    ),
    points.curve(data[[response]], data[[predictor]])[, 2]
  )
  expect_equal(
    rev(
      calc_fpr(
        data,
        thresholds = get_thresholds(
          predictor = data[[predictor]],
        ),
        response = disease,
        predictor = ENSG00000000003.15
      )
    ),
    points.curve(data[[response]], data[[predictor]])[, 1]
  )
  expect_equal(
    rev(
      calc_ratios(
        data,
        thresholds = get_thresholds(
          predictor = data[[predictor]],
        ),
        response = disease,
        predictor = ENSG00000000003.15
      )[["fpr"]]
    ),
    points.curve(data[[response]], data[[predictor]])[, 1]
  )
  expect_equal(
    rev(
      calc_ratios(
        data,
        thresholds = get_thresholds(
          predictor = data[[predictor]],
        ),
        response = disease,
        predictor = ENSG00000000003.15
      )[["tpr"]]
    ),
    points.curve(data[[response]], data[[predictor]])[, 2]
  )
})

test_that("sorting when getting points not needed", {
  points_calc_ratios <- as.data.frame(
    calc_ratios(
      thresholds = get_thresholds(
        predictor = data[[predictor]],
      ),
      response = data[[response]],
      predictor = data[[predictor]]
    )
  )
  points_points_curve <- data.frame(
    fpr = points.curve(data[[response]], data[[predictor]])[, 1],
    tpr = points.curve(data[[response]], data[[predictor]])[, 2]
  )
  expect_true(dplyr::setequal(points_calc_ratios, points_points_curve))
})

test_that("get_thresholds is faster/equal than points.thresholds", {
  expect_faster(
    get_thresholds(predictor = data[[predictor]]),
    points.thresholds(data[[response]], data[[predictor]])
  )
})

test_that("roc_points is faster/equal than points.curve", {
  skip()
  expect_faster(
    roc_points(NULL, data[["disease"]], data[[predictor]]),
    points.curve(data[["disease"]], data[[predictor]])
  )
})