#' @rdname specificity_indexes
#' @export
#' @references
#' McClish D. K. Analyzing a Portion of the ROC Curve. *Medical Decision Making*
#' 9, 190-195 (1989).
sp_auc <- function(
    data = NULL,
    response,
    predictor,
    lower_fpr,
    upper_fpr,
    .condition = NULL,
    .invalid = FALSE) {
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
  pauc <- pauc_fpr(partial_fpr, partial_tpr)
  lower_bound <- calc_fpr_diagonal_lower_bound(partial_fpr, partial_tpr)
  upper_bound <- upper_fpr - lower_fpr

  if (pauc < lower_bound && !.invalid) {
    # TODO: improve warning when pauc < under diagonal
    warning("Curve under chance line")
    spauc <- NA
  } else {
    spauc <- (1 + ((pauc - lower_bound) / (upper_bound - lower_bound))) / 2
  }
  spauc
}
