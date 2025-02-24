#' @rdname sensitivity_indexes
#' @references
#' Jiang Y., Metz C. E. y Nishikawa R. M. A receiver operating characteristic
#' partial area index for highly sensitive diagnostic tests. *Radiology*
#' 201, 745-750 (1996).
#' @export
np_auc <- function(data = NULL,
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

  npauc <- pauc / (1 - lower_tpr)
  return(npauc)
}
