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
    upper_fpr) {
  if (!is.null(data)) {
    response <- pull(data, {{ response }})
    predictor <- pull(data, {{ predictor }})
  }
  auc(
    response = response,
    predictor = predictor,
    direction = "<",
    quiet = TRUE,
    partial.auc = c(1 - upper_fpr, 1 - lower_fpr),
    partial.auc.focus = "spec",
    partial.auc.correct = TRUE
  )
}
