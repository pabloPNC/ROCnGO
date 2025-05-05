summarize_tpr_predictor <- function(data = NULL,
                                    predictor,
                                    response,
                                    threshold,
                                    .condition = NULL) {
  if (!is.null(data)) {
    predictor <- data %>% pull({{ predictor }})
    response <- data %>% pull({{ response }})
  }

  response <- as_response(response, .condition)

  tpr_fpr <- roc_points(NULL, response, predictor)
  ptpr_pfpr <- calc_partial_roc_points(
    tpr = tpr_fpr$tpr,
    fpr = tpr_fpr$fpr,
    lower_threshold = threshold,
    upper_threshold = 1,
    ratio = "tpr"
  )
  tibble(
    auc = auc(
      response = response,
      predictor = predictor
    ),
    pauc = pauc_tpr(
      partial_tpr = ptpr_pfpr$partial_tpr,
      partial_fpr = ptpr_pfpr$partial_fpr
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
                                    threshold,
                                    .condition = NULL) {
  if (!is.null(data)) {
    predictor <- data %>% pull({{ predictor }})
    response <- data %>% pull({{ response }})
  }
  response <- as_response(response, .condition)
  tpr_fpr <- roc_points(NULL, response, predictor)
  ptpr_pfpr <- calc_partial_roc_points(
    tpr = tpr_fpr$tpr,
    fpr = tpr_fpr$fpr,
    lower_threshold = 0,
    upper_threshold = threshold,
    ratio = "fpr"
  )
  tibble(
    auc = auc(
      response = response,
      predictor = predictor,
    ),
    pauc = pauc_fpr(
      partial_tpr = ptpr_pfpr$partial_tpr,
      partial_fpr = ptpr_pfpr$partial_fpr
    ),
    sp_auc = sp_auc(
      response = response,
      predictor = predictor,
      lower_fpr = 0,
      upper_fpr = threshold
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
#' Calculates a series of metrics describing global and local
#' classifier performance.
#' @inheritParams calc_partial_roc_points
#' @param threshold A number between 0 and 1, both inclusive, which represents
#' the region bound where to calculate partial area under curve.
#'
#' If `ratio = "tpr"`, it represents lower bound of the TPR region, being its
#' upper limit equal to 1.
#'
#' If `ratio = "fpr"`, it represents the upper bound of the FPR region,
#' being its lower limit equal to 0.
#'
#' @returns
#' A single row tibble with different predictor with following metrics as
#' columns:
#'
#' * Area under curve (AUC) as a metric of global performance.
#' * Partial are under curve (pAUC) as a metric of local performance.
#' * Indexes derived from pAUC, depending on the selected ratio.
#' [Sensitivity indexes][ROCnGO::sensitivity_indexes] will be used for
#' TPR and [specificity indexes][ROCnGO::specificity_indexes] for FPR.
#' * [Curve shape][ROCnGO::calc_curve_shape] in the specified region.
#' @examples
#' # Summarize Sepal.Width as a classifier of setosa species
#' # and local performance in TPR (0.9, 1)
#' summarize_predictor(
#'  data = iris,
#'  predictor = Sepal.Width,
#'  response = Species,
#'  ratio = "tpr",
#'  threshold = 0.9
#' )
#' # Summarize Sepal.Width as a classifier of setosa species
#' # and local performance in FPR (0, 0.1)
#' summarize_predictor(
#'  data = iris,
#'  predictor = Sepal.Width,
#'  response = Species,
#'  ratio = "fpr",
#'  threshold = 0.1
#' )
#' @export
summarize_predictor <- function(data = NULL,
                                predictor,
                                response,
                                ratio,
                                threshold,
                                .condition = NULL) {
  resignal_thresholds({
    if (ratio == "tpr") {
      summarize_tpr_predictor(
        data,
        {{ predictor }},
        {{ response }},
        threshold,
        .condition
      )
    } else if (ratio == "fpr") {
      summarize_fpr_predictor(
        data,
        {{ predictor }},
        {{ response }},
        threshold,
        .condition
      )
    }
  })
}

#' @title Summarize classifiers performance in a dataset
#' @description
#' Calculate a series of metrics describing global and local
#' performance for selected classifiers in a dataset.
#' @inheritParams summarize_predictor
#' @param predictors A vector of numeric data variables which represents the
#' different classifiers or predictors in data to be summarized.
#'
#' If `NULL`and by default, `predictors` will match all numeric variables in
#' `data` with the exception of `response`, given that it has a numeric type.
#' @param .progress If `TRUE`, show progress of calculations.
#' @inherit roc_points details
#' @returns
#' A list with different elements:
#' * Performance metrics for each of evaluated classifiers.
#' * Overall description of performance metrics in the dataset.
#' @examples
#' summarize_dataset(iris, response = Species, ratio = "tpr", threshold = 0.9)
#' @export
summarize_dataset <- function(data,
                              predictors = NULL,
                              response,
                              ratio,
                              threshold,
                              .condition = NULL,
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

  resignal_thresholds({
    for (i in 1:length(predictors_dataset)) {
      if (ratio == "tpr") {
        result <- summarize_tpr_predictor(
          NULL,
          predictors_dataset[[i]],
          response,
          threshold,
          .condition
        )
      } else if (ratio == "fpr") {
        result <- summarize_fpr_predictor(
          NULL,
          predictors_dataset[[i]],
          response,
          threshold,
          .condition
        )
      }

      id <- names(predictors_dataset[i])
      results[[id]] <- result

      if (.progress == TRUE) {
        print(str_glue("[*] {length(results)}/{length(predictors_dataset)}"))
      }
    }
  })

  metrics <- list(
    data = bind_rows(results, .id = "identifier")
  )
  metrics[["curve_shape"]] <- metrics$data %>%
    group_by(.data$curve_shape) %>%
    summarize(count = n())

  metrics[["auc"]] <- metrics$data %>%
    group_by(
      auc > 0.5,
      auc > 0.8
    ) %>%
    summarize(count = n())

  return(metrics)
}
