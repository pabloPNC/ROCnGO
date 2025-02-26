summarize_tpr_predictor <- function(data = NULL,
                                    predictor,
                                    response,
                                    threshold) {
  if (!is.null(data)) {
    predictor <- data %>% pull({{ predictor }})
    response <- data %>% pull({{ response }})
  }

  response <- as_response(response)

  tpr_fpr <- roc_points(NULL, response, predictor)
  ptpr_pfpr <- calc_partial_roc_points(
    tpr = tpr_fpr$tpr,
    fpr = tpr_fpr$fpr,
    lower_threshold = threshold,
    upper_threshold = 1,
    ratio = "tpr"
  )
  tibble(
    auc = as.double(
      auc(
        response = response,
        predictor = predictor,
        direction = "<",
        quiet = TRUE
      )
    ),
    pauc = as.double(
      auc(
        response = response,
        predictor = predictor,
        direction = "<",
        quiet = TRUE,
        partial.auc = c(threshold, 1),
        partial.auc.focus = "sens"
      )
    ),
    np_auc = np_auc(NULL, response, predictor, threshold),
    fp_auc = fp_auc(NULL, response, predictor, threshold),
    curve_shape = calc_tpr_curve_shape(
      ptpr_pfpr$partial_fpr,
      ptpr_pfpr$partial_tpr
    )
  )
}

summarize_fpr_predictor <- function(data = NULL,
                                    predictor,
                                    response,
                                    threshold) {
  if (!is.null(data)) {
    predictor <- data %>% pull({{ predictor }})
    response <- data %>% pull({{ response }})
  }
  response <- as_response(response)
  tpr_fpr <- roc_points(NULL, response, predictor)
  ptpr_pfpr <- calc_partial_roc_points(
    tpr = tpr_fpr$tpr,
    fpr = tpr_fpr$fpr,
    lower_threshold = 0,
    upper_threshold = threshold,
    ratio = "fpr"
  )
  tibble(
    auc = as.double(
      auc(
        response = response,
        predictor = predictor,
        direction = "<",
        quiet = TRUE
      )
    ),
    pauc = as.double(
      auc(
        response = response,
        predictor = predictor,
        direction = "<",
        quiet = TRUE,
        partial.auc = c(1 - threshold, 1),
        partial.auc.focus = "spec"
      )
    ),
    sp_auc = as.double(
      auc(
        response = response,
        predictor = predictor,
        direction = "<",
        quiet = TRUE,
        partial.auc = c(1 - threshold, 1),
        partial.auc.focus = "spec",
        partial.auc.correct = TRUE,
        allow.invalid.partial.auc.correct = FALSE
      )
    ),
    tp_auc = tp_auc(NULL, response, predictor, 0, threshold),
    curve_shape = calc_fpr_curve_shape(
      ptpr_pfpr$partial_fpr,
      ptpr_pfpr$partial_tpr
    )
  )
}

#' @title Summarize classifier performance
#' @description
#' Calculates a series of metrics describing global classifier performance and
#' performance over a region of interest.
#' @inheritParams roc_points
#' @param ratio Ratio in which to apply calculations. If `"tpr"` they will be
#' applied over TPR ratio, if `"fpr"` it will be calculated over FPR ratio.
#' @param threshold Theshold in which to make partial area calculations. When
#' working in TPR it represents lower threshold up to 1, and upper threshold
#' in FPR up to 0.
#' @inheritSection roc_points Methods
#' @inheritSection roc_points Data masking variables
#' @details
#' Metrics calculated to evaluate are the following:
#'
#' * Area under curve (AUC) as a metric of global performances.
#' * Partial area under curve (pAUC) as a metric of local performance.
#' * Indexes derived from pAUC, depending on the selected ratio.
#' [Sensitivity indexes][ROCnGO::sensitivity_indexes] will be used for
#' TPR and [specificity indexes][ROCnGO::specificity_indexes] for FPR.
#' * [Curve shape][ROCnGO::calc_curve_shape] in the specified region.
#' @returns
#' A single row tibble with different predictor metrics as columns.
#' @export
summarize_predictor <- function(data = NULL,
                                predictor,
                                response,
                                ratio,
                                threshold) {
  if (ratio == "tpr") {
    summarize_tpr_predictor(
      data,
      {{ predictor }},
      {{ response }},
      threshold
    )
  } else if (ratio == "fpr") {
    summarize_fpr_predictor(
      data,
      {{ predictor }},
      {{ response }},
      threshold
    )
  }
}

#' @title Summarize classifiers performance in a dataset
#' @description
#' Calculate a series of metrics describing global classifier performance and
#' performance over a region of interest for selected classifiers in a dataset.
#' @inheritParams summarize_predictor
#' @param predictors A vector of data variables. Each variable must be a factor,
#' integer or character vector representing the class to predict on each
#' observation or *Gold Standard*. For more info on how to select class of
#' interest see *Methods*.
#' @param .progress If `TRUE`, show progress of calculations.
#' @inheritSection roc_points Methods
#' @inheritSection roc_points Data masking variables
#' @inherit roc_points details
#' @returns
#' List with different keys with different metrics for each of the specified
#' classifiers in dataset.
#' @export
summarize_dataset <- function(data,
                              predictors = NULL,
                              response,
                              ratio,
                              threshold,
                              .progress = FALSE) {
  results <- list()
  predictors_expr <- enquo(predictors)

  if (!quo_is_null(predictors_expr)) {
    predictors_dataset <- data %>%
      select({{ predictors }})
  } else {
    predictors_dataset <- data %>%
      select((where(is.integer) | where(is.double)) & !{{ response }})
  }

  response <- data %>% pull({{ response }})

  for (i in 1:length(predictors_dataset)) {
    if (ratio == "tpr") {
      result <- summarize_tpr_predictor(
        NULL,
        predictors_dataset[[i]],
        response,
        threshold
      )
    } else if (ratio == "fpr") {
      result <- summarize_fpr_predictor(
        NULL,
        predictors_dataset[[i]],
        response,
        threshold
      )
    }

    id <- names(predictors_dataset[i])
    results[[id]] <- result

    if (.progress == TRUE) {
      print(str_glue("[*] {length(results)}/{length(predictors_dataset)}"))
    }
  }

  metrics <- list(
    data = bind_rows(results, .id = "identifier")
  )
  metrics[["curve_shape"]] <- metrics$data %>%
    group_by(curve_shape) %>%
    summarize(count = n())

  metrics[["auc"]] <- metrics$data %>%
    group_by(
      auc > 0.5,
      auc > 0.8
    ) %>%
    summarize(count = n())

  return(metrics)
}
