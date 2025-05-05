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
  if (!is.null(data)) {
    tpr_fpr <- data %>% roc_points({{ response }}, {{ predictor }}, .condition)
    tpr <- tpr_fpr %>% pull(tpr)
    fpr <- tpr_fpr %>% pull(fpr)
  } else {
    tpr_fpr <- NULL %>% roc_points(response, predictor, .condition)
    tpr <- tpr_fpr[["tpr"]]
    fpr <- tpr_fpr[["fpr"]]
  }

  partial_tpr_fpr <- calc_partial_roc_points(
    tpr = tpr,
    fpr = fpr,
    lower_threshold = lower_tpr,
    upper_threshold = 1,
    ratio = "tpr"
  )

  partial_tpr <- partial_tpr_fpr[["partial_tpr"]]
  partial_fpr <- partial_tpr_fpr[["partial_fpr"]]

  pauc <- sum(
    diff(partial_tpr) *
      apply(
        cbind(
          1 - partial_fpr[-1],
          1 - partial_fpr[-length(partial_tpr)]
        ),
        1,
        mean
      )
  )

  bounds <- calc_tpr_bounds(partial_fpr, partial_tpr)
  lower_bound <- bounds[["lower_bound"]]
  upper_bound <- bounds[["upper_bound"]]

  fpauc <- (1 + ((pauc - lower_bound) / (upper_bound - lower_bound))) / 2
  return(fpauc)
}
