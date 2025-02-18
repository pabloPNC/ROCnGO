#' @importFrom pROC auc
#' @importFrom dplyr pull
#' @export
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
