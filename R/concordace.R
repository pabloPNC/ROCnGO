#' @export
cp_auc <- function(data = NULL,
                   response,
                   predictor,
                   lower_fpr,
                   upper_fpr,
                   .condition = NULL) {
  ppoints <- calc_partial_roc_points(
    data = data,
    response = {{ response }},
    predictor = {{ predictor }},
    .condition = {{ .condition }},
    lower_threshold = lower_fpr,
    upper_threshold = upper_fpr,
    ratio = "fpr"
  )
  pauc_fpr <- pauc_fpr(ppoints$partial_fpr, ppoints$partial_tpr)
  pauc_tpr <- pauc_tpr(ppoints$partial_fpr, ppoints$partial_tpr)
  c_pauc <- (0.5 * pauc_fpr) + (0.5 * pauc_tpr)
  c_pauc
}

#' @export
ncp_auc <- function(data = NULL,
                    response,
                    predictor,
                    lower_fpr,
                    upper_fpr,
                    .condition = NULL) {
  c_pauc <- cp_auc(
    data = data,
    response = {{ response }},
    predictor = {{ predictor }},
    .condition = {{ .condition }},
    lower_fpr = lower_fpr,
    upper_fpr = upper_fpr
  )
  ppoints <- calc_partial_roc_points(
    data = data,
    response = {{ response }},
    predictor = {{ predictor }},
    .condition = {{ .condition }},
    lower_threshold = lower_fpr,
    upper_threshold = upper_fpr,
    ratio = "fpr"
  )
  fpr_range <- ppoints$partial_fpr[nrow(ppoints)] - ppoints$partial_fpr[1]
  tpr_range <- ppoints$partial_tpr[nrow(ppoints)] - ppoints$partial_tpr[1]
  nc_pauc <- c_pauc / (0.5 * (tpr_range + fpr_range))
  nc_pauc
}
