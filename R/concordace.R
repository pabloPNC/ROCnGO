#' @title Concordance indexes
#' @description
#' Concordance derived indexes allow calculation and explanation of area under
#' ROC curve in a specific region. They use a dual perspective since they
#' consider both TPR and FPR ranges which enclose the region of interest.
#'
#' `cp_auc()` applies *concordan partial area under curve* (CpAUC), while
#' `ncp_auc()` applies its normalized version by dividing by the total area.
#' @inheritParams calc_partial_roc_points
#' @returns
#' A numeric value representing index score for the partial area under ROC
#' curve.
#' @name concordance_indexes
#' @examples
#' # Calculate cp_auc of Sepal.Width as a classifier of setosa especies in
#' # FPR = (0, 0.1)
#' cp_auc(
#'   iris,
#'   response = Species,
#'   predictor = Sepal.Width,
#'   lower_threshold = 0,
#'   upper_threshold = 0.1,
#'   ratio = "fpr"
#' )
#' # Calculate ncp_auc of Sepal.Width as a classifier of setosa especies in
#' # FPR = (0, 0.1)
#' ncp_auc(
#'   iris,
#'   response = Species,
#'   predictor = Sepal.Width,
#'   lower_threshold = 0,
#'   upper_threshold = 0.1,
#'   ratio = "fpr"
#' )
NULL

#' @rdname concordance_indexes
#' @references
#' Carrington, André M., et al. A new concordant partial AUC and partial c
#' statistic for imbalanced data in the evaluation of machine learning
#' algorithms. *BMC medical informatics and decision making* 20 (2020): 1-12.
#' @export
cp_auc <- function(data = NULL,
                   response,
                   predictor,
                   lower_threshold,
                   upper_threshold,
                   ratio,
                   .condition = NULL) {
  ppoints <- calc_partial_roc_points(
    data = data,
    response = {{ response }},
    predictor = {{ predictor }},
    .condition = {{ .condition }},
    lower_threshold = lower_threshold,
    upper_threshold = upper_threshold,
    ratio = ratio
  )
  pauc_fpr <- pauc_fpr(ppoints$fpr, ppoints$tpr)
  pauc_tpr <- pauc_tpr(ppoints$fpr, ppoints$tpr)
  c_pauc <- (0.5 * pauc_fpr) + (0.5 * pauc_tpr)
  c_pauc
}

#' @rdname concordance_indexes
#' @export
ncp_auc <- function(data = NULL,
                    response,
                    predictor,
                    lower_threshold,
                    upper_threshold,
                    ratio,
                    .condition = NULL) {
  c_pauc <- cp_auc(
    data = data,
    response = {{ response }},
    predictor = {{ predictor }},
    .condition = {{ .condition }},
    lower_threshold = lower_threshold,
    upper_threshold = upper_threshold,
    ratio = ratio
  )
  ppoints <- calc_partial_roc_points(
    data = data,
    response = {{ response }},
    predictor = {{ predictor }},
    .condition = {{ .condition }},
    lower_threshold = lower_threshold,
    upper_threshold = upper_threshold,
    ratio = ratio
  )
  fpr_range <- ppoints$fpr[nrow(ppoints)] - ppoints$fpr[1]
  tpr_range <- ppoints$tpr[nrow(ppoints)] - ppoints$tpr[1]
  nc_pauc <- c_pauc / (0.5 * (tpr_range + fpr_range))
  nc_pauc
}
