#' @title Calculate area under ROC curve
#' @description
#' Calculates area under curve (AUC) of a predictor's ROC curve.
#' @inheritParams roc_points
#' @returns
#' A numerical value representing the area under ROC curve.
#' @examples
#' # Calc AUC of Sepal.Width as a classifier of setosa species
#' auc(iris, Species, Sepal.Width)
#' # Change class to predict to virginica
#' auc(iris, Species, Sepal.Width, .condition = "virginica")
#' @export
auc <- function(data = NULL,
                response,
                predictor,
                .condition = NULL) {
  UseMethod("auc", data)
}

#' @export
auc.ratio_df <- function(data = NULL,
                         response,
                         predictor,
                         .condition = NULL) {
  height <- diff(data[["fpr"]])
  bases <- apply(
    cbind(
      data[["tpr"]][-1],
      data[["tpr"]][-length(data[["tpr"]])]
    ),
    MARGIN = 1,
    FUN = mean
  )
  sum(height * bases)
}

#' @export
auc.NULL <- function(data = NULL,
                     response,
                     predictor,
                     .condition = NULL) {
  ratios <- roc_points(NULL, response, predictor, .condition) %>%
    arrange(.data[["fpr"]], .data[["tpr"]])
  auc.ratio_df(ratios, .condition = .condition)
}

#' @export
auc.data.frame <- function(data = NULL,
                           response,
                           predictor,
                           .condition = NULL) {
  response <- pull(data, {{ response }})
  predictor <- pull(data, {{ predictor }})
  auc.NULL(NULL, response, predictor, .condition)
}
