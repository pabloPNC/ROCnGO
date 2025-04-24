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
  pauc_fpr <- pauc_fpr(ppoints$partial_fpr, ppoints$partial_tpr)
  pauc_tpr <- pauc_tpr(ppoints$partial_fpr, ppoints$partial_tpr)
  c_pauc <- (0.5 * pauc_fpr) + (0.5 * pauc_tpr)
  c_pauc
}

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
  fpr_range <- ppoints$partial_fpr[nrow(ppoints)] - ppoints$partial_fpr[1]
  tpr_range <- ppoints$partial_tpr[nrow(ppoints)] - ppoints$partial_tpr[1]
  nc_pauc <- c_pauc / (0.5 * (tpr_range + fpr_range))
  nc_pauc
}
