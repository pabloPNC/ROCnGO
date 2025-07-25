#' @param .invalid If `FALSE`, the default, `sp_auc()` will return `NA` when
#' ROC curve does not fit theoretical bounds and index cannot be applied.
#' If `TRUE`, function will force the calculation and return a value despite
#' probably being incorrect.
#' @rdname specificity_indexes
#' @export
#' @references
#' McClish D. K. Analyzing a Portion of the ROC Curve. *Medical Decision Making*
#' 9, 190-195 (1989).
sp_auc <- function(data = NULL,
                   response,
                   predictor,
                   lower_fpr,
                   upper_fpr,
                   .condition = NULL,
                   .invalid = FALSE) {
  UseMethod("sp_auc", data)
}

#' @export
sp_auc.ratio_df <- function(data = NULL,
                            response,
                            predictor,
                            lower_fpr,
                            upper_fpr,
                            .condition = NULL,
                            .invalid = FALSE) {
  pauc <- pauc_fpr(data$fpr, data$tpr)
  lower_bound <- calc_fpr_diagonal_lower_bound(
    data$fpr,
    data$tpr
  )
  upper_bound <- max(data$fpr) - min(data$fpr)

  if (pauc < lower_bound && !.invalid) {
    warn_under_chance()
    spauc <- NA
  } else {
    spauc <- (1 + ((pauc - lower_bound) / (upper_bound - lower_bound))) / 2
  }
  spauc
}

#' @export
sp_auc.NULL <- function(data = NULL,
                        response,
                        predictor,
                        lower_fpr,
                        upper_fpr,
                        .condition = NULL,
                        .invalid = FALSE) {
  ratios <- roc_points(NULL, response, predictor, .condition) %>%
    arrange(.data[["fpr"]], .data[["tpr"]])
  pratios <- calc_partial_roc_points(
    data = ratios,
    lower_threshold = lower_fpr,
    upper_threshold = upper_fpr,
    ratio = "fpr"
  )
  sp_auc.ratio_df(pratios, .condition = .condition, .invalid = .invalid)
}

#' @export
sp_auc.data.frame <- function(data = NULL,
                              response,
                              predictor,
                              lower_fpr,
                              upper_fpr,
                              .condition = NULL,
                              .invalid = FALSE) {
  response <- pull(data, {{ response }})
  predictor <- pull(data, {{ predictor }})
  sp_auc.NULL(
    NULL,
    response,
    predictor,
    lower_fpr,
    upper_fpr,
    .condition,
    .invalid
  )
}
