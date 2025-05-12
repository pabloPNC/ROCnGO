#' @rdname sensitivity_indexes
#' @references
#' Jiang Y., Metz C. E. y Nishikawa R. M. A receiver operating characteristic
#' partial area index for highly sensitive diagnostic tests. *Radiology*
#' 201, 745-750 (1996).
#' @export
np_auc <- function(data,
                   response,
                   predictor,
                   lower_tpr,
                   .condition = NULL) {
  UseMethod("np_auc", data)
}

#' @export
np_auc.ratio_df <- function(data,
                            response,
                            predictor,
                            lower_tpr,
                            .condition = NULL) {
  pauc <- pauc_tpr(data$fpr, data$tpr)
  npauc <- pauc / (1 - min(data$tpr))
  npauc
}

#' @export
np_auc.NULL <- function(data,
                        response,
                        predictor,
                        lower_tpr,
                        .condition = NULL) {
  ratios <- roc_points(NULL, response, predictor, .condition)
  pratios <- calc_partial_roc_points(
    tpr = ratios$tpr,
    fpr = ratios$fpr,
    lower_threshold = lower_tpr,
    upper_threshold = 1,
    ratio = "tpr"
  )
  np_auc.ratio_df(pratios, .condition = .condition)
}


#' @export
np_auc.data.frame <- function(data,
                              response,
                              predictor,
                              lower_tpr,
                              .condition = NULL) {
  response <- pull(data, {{ response }})
  predictor <- pull(data, {{ predictor }})
  np_auc.NULL(
    NULL,
    response,
    predictor,
    lower_tpr,
    .condition
  )
}
