#' @title Specificity indexes
#' @description
#' Specificity indexes provide different ways of calculating area under
#' ROC curve in a specific FPR region. Two different approaches to calculate
#' this area are available:
#' * `tp_auc()` applies *tighter partial area under curve* index (SpAUC).
#' This one calculates area under curve adjusting to points defined by the curve
#' in the selected region.
#' * `sp_auc()` applies *standardized partial area under curve* index (TpAUC),
#' which calculates area under curve over the whole specified region.
#' @inheritParams roc_points
#' @param lower_fpr,upper_fpr Two numbers between 0 and 1, inclusive. These
#' numbers represent lower and upper values of FPR region where to calculate
#' partial area under curve.
#' @inherit sensitivity_indexes return
#' @name specificity_indexes
#' @examples
#' # Calculate sp_auc of Sepal.Width as a classifier of setosa species
#' # in FPR = (0.9, 1)
#' sp_auc(
#'  iris,
#'  response = Species,
#'  predictor = Sepal.Width,
#'  lower_fpr = 0,
#'  upper_fpr = 0.1
#' )
#' # Calculate tp_auc of Sepal.Width as a classifier of setosa species
#'  # in FPR = (0.9, 1)
#' tp_auc(
#'  iris,
#'  response = Species,
#'  predictor = Sepal.Width,
#'  lower_fpr = 0,
#'  upper_fpr = 0.1
#' )
NULL

#' @rdname specificity_indexes
#' @references
#' Vivo J.-M., Franco M. y Vicari D. Rethinking an ROC partial area index for
#' evaluating the classification performance at a high specificity range.
#' *Advances in Data Analysis and Classification* 12, 683-704 (2018).
#' @export
tp_auc <- function(data = NULL,
                   response,
                   predictor,
                   lower_fpr,
                   upper_fpr,
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
    lower_threshold = lower_fpr,
    upper_threshold = upper_fpr,
    ratio = "fpr"
  )

  partial_tpr <- partial_tpr_fpr[["partial_tpr"]]
  partial_fpr <- partial_tpr_fpr[["partial_fpr"]]

  pauc <- sum(
    diff(partial_fpr) *
      apply(
        cbind(
          partial_tpr[-1],
          partial_tpr[-length(partial_tpr)]
        ),
        1,
        mean
      )
  )
  bounds <- calc_fpr_bounds(partial_fpr, partial_tpr)
  lower_bound <- bounds[["lower_bound"]]
  upper_bound <- bounds[["upper_bound"]]

  tpauc <- (1 + ((pauc - lower_bound) / (upper_bound - lower_bound))) / 2
  return(tpauc)
}
