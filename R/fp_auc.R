#' @title Sensitivity indexes
#' @description
#' Sensitivity indexes provide different ways of calculating area under
#' ROC curve in a specific TPR region. Two different approaches to calculate
#' this area are available:
#' * `fp_auc()` applies *fitted partial area under curve* index (FpAUC). This
#' one calculates area under curve adjusting to points defined by the curve
#' in the selected region.
#' * `np_auc()` applies *normalized partial area under curve* index (NpAUC),
#' which calculates area under curve over the whole specified region.
#' @inheritParams roc_points
#' @param lower_tpr A numeric value between 0 and 1, inclusive, which represents
#' lower value of TPR for the region where to calculate the
#' partial area under curve.
#'
#' Because of definition of sensitivity indexes, upper bound of the region will
#' be established as 1.
#' @returns
#' A numeric value representing the index score for the partial area under
#' ROC curve.
#' @name sensitivity_indexes
#' @examples
#' # Calculate fp_auc of Sepal.Width as a classifier of setosa species
#' # in TPR = (0.9, 1)
#' fp_auc(iris, response = Species, predictor = Sepal.Width, lower_tpr = 0.9)
#' # Calculate np_auc of Sepal.Width as a classifier of setosa species
#' # in TPR = (0.9, 1)
#' np_auc(iris, response = Species, predictor = Sepal.Width, lower_tpr = 0.9)
NULL

#' @rdname sensitivity_indexes
#' @references
#' Franco M. y Vivo J.-M. Evaluating the Performances of Biomarkers over a
#' Restricted Domain of High Sensitivity. *Mathematics* 9, 2826 (2021).
#' @export
fp_auc <- function(data = NULL,
                   response,
                   predictor,
                   lower_tpr,
                   .condition = NULL) {
  UseMethod("fp_auc", data)
}

#' @export
fp_auc.ratio_df <- function(data = NULL,
                            response,
                            predictor,
                            lower_tpr,
                            .condition = NULL) {
  pauc <- pauc_tpr(data$fpr, data$tpr)
  bounds <- calc_tpr_bounds(data$fpr, data$tpr)
  lower_bound <- bounds$lower_bound
  upper_bound <- bounds$upper_bound
  fpauc <- (1 + ((pauc - lower_bound) / (upper_bound - lower_bound))) / 2
  fpauc
}

#' @export
fp_auc.NULL <- function(data = NULL,
                        response,
                        predictor,
                        lower_tpr,
                        .condition = NULL) {
  ratios <- roc_points(NULL, response, predictor, .condition) %>%
    arrange(.data[["fpr"]], .data[["tpr"]])
  pratios <- calc_partial_roc_points(
    data = ratios,
    lower_threshold = lower_tpr,
    upper_threshold = 1,
    ratio = "tpr"
  )
  fp_auc.ratio_df(pratios, .condition = .condition)
}

#' @export
fp_auc.data.frame <- function(data = NULL,
                              response,
                              predictor,
                              lower_tpr,
                              .condition = NULL) {
  predictor <- pull(data, {{ predictor }})
  response <- pull(data, {{ response }})
  fp_auc.NULL(
    NULL,
    response,
    predictor,
    lower_tpr,
    .condition
  )
}
