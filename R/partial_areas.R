#' @title Calculate partial area under curve
#' @description
#' Calculates area under curve curve in an specific TPR or FPR region.
#' @inheritParams calc_partial_roc_points
#' @returns
#' A numeric value representing the area under ROC curve in the specified region.
#' @examples
#' # Calculate pauc of Sepal.Width as a classifier of setosa species in
#' # in TPR = (0.9, 1)
#' pauc(
#'   iris,
#'   response = Species,
#'   predictor = Sepal.Width,
#'   ratio = "tpr",
#'   lower_threshold = 0.9,
#'   upper_threshold = 1
#' )
#' # Calculate pauc of Sepal.Width as a classifier of setosa species in
#' # in FPR = (0, 0.1)
#' pauc(
#'   iris,
#'   response = Species,
#'   predictor = Sepal.Width,
#'   ratio = "fpr",
#'   lower_threshold = 0,
#'   upper_threshold = 0.1
#' )
#' @export
pauc <- function(data = NULL,
                 response,
                 predictor,
                 ratio,
                 lower_threshold,
                 upper_threshold,
                 .condition = NULL) {
  if (!is.null(data)) {
    ratios <- roc_points(data, {{ response }}, {{ predictor }}, .condition)
  } else {
    ratios <- roc_points(NULL, response, predictor, .condition)
  }
  tpr <- ratios$tpr
  fpr <- ratios$fpr
  partial_ratios <- calc_partial_roc_points(
    tpr = tpr,
    fpr = fpr,
    lower_threshold = lower_threshold,
    upper_threshold = upper_threshold,
    ratio = ratio
  )
  ptpr <- partial_ratios[["tpr"]]
  pfpr <- partial_ratios[["fpr"]]

  if (ratio == "tpr") {
    pauc <- pauc_tpr(partial_tpr = ptpr, partial_fpr = pfpr)
  } else if (ratio == "fpr") {
    pauc <- pauc_fpr(partial_tpr = ptpr, partial_fpr = pfpr)
  } else {
    # TODO: improve error message
    stop("`ratio` arg should take `'tpr'` or `'fpr'` value")
  }
  pauc
}

pauc_fpr <- function(partial_fpr, partial_tpr) {
  pauc <- sum(
    diff(partial_fpr) *
      apply(
        cbind(
          partial_tpr[-1],
          partial_tpr[-length(partial_tpr)]
        ),
        MARGIN = 1,
        FUN = mean
      )
  )
  pauc
}

pauc_tpr <- function(partial_fpr, partial_tpr) {
  pauc <- sum(
    diff(partial_tpr) *
      apply(
        cbind(
          1 - partial_fpr[-1],
          1 - partial_fpr[-length(partial_tpr)]
        ),
        MARGIN = 1,
        FUN = mean
      )
  )
  pauc
}
