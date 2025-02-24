#' @title Sensitivity indexes
#' @description
#' Sensitivity indexes provide different ways of calculating partial area under
#' ROC curve in a specific TPR range. Two different approaches to calculate this
#' area are available:
#' * `fp_aup()` applies *fitted partial area under curve* index (FpAUC). This
#' one calculates area under curve adjusting to points defined by curve in
#' selected region.
#' * `np_auc()` applies *normalized partial area under curve* index (NpAUC),
#' which calculates area under curve over the whole specified region.
#' @inheritParams roc_points
#' @inheritSection roc_points Methods
#' @inheritSection roc_points Data masking variables
#' @param lower_tpr Lower value of TPR for the region in which to calculate the
#' partial area under curve.
#' @returns
#' A numeric indicating the partial area under curve in the selected region.
#' @name sensitivity_indexes
#' @examples
#' NULL
NULL

#' @rdname sensitivity_indexes
#' @references
#' Franco M. y Vivo J.-M. Evaluating the Performances of Biomarkers over a
#' Restricted Domain of High Sensitivity. *Mathematics* 9, 2826 (2021).
#' @export
fp_auc <- function(data = NULL,
                   response,
                   predictor,
                   lower_tpr) {
  if (!is.null(data)) {
    tpr_fpr <- data %>% roc_points({{ response }}, {{ predictor }})
    tpr <- tpr_fpr %>% pull(tpr)
    fpr <- tpr_fpr %>% pull(fpr)
  } else {
    tpr_fpr <- roc_points(NULL, response, predictor)
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
