plot_roc <- function(data) {
  ggplot(data) +
    labs(x = "FPR", y = "TPR", color = "Predictor", fill = "Bound")
}

add_roc_geom <- function(data, geom, .label) {
  geom(
    data = data,
    mapping = aes(
      x = .data[["fpr"]],
      y = .data[["tpr"]],
      color = .label
    ),
    size = 0.8
  )
}

plot_roc_geom <- function(data, geom, .label) {
  plot_roc(data) +
    add_roc_geom(data, geom, .label)
}

#' @title Plot classifier points of a ROC curve
#' @description
#' Create an scatter plot using ROC curve points.
#' @inheritParams calc_partial_roc_points
#' @param .label A string representing the name used in labels.
#'
#' If `NULL`, variable name from `predictor` will be used as label.
#' @examples
#' plot_roc_points(iris, response = Species, predictor = Sepal.Width)
#' @export
plot_roc_points <- function(data,
                            response = NULL,
                            predictor = NULL,
                            .condition = NULL,
                            .label = NULL) {
  UseMethod("plot_roc_points", data)
}

#' @export
plot_roc_points.data.frame <- function(data,
                                       response = NULL,
                                       predictor = NULL,
                                       .condition = NULL,
                                       .label = NULL) {

  if (is.null(.label)) {
    predictor_name <- as_name(enquo(predictor))
  } else {
    predictor_name <- .label
  }

  response <- pull(data, {{ response }})
  predictor <- pull(data, {{ predictor }})

  plot_roc_points.NULL(NULL, response, predictor, .condition, predictor_name)
}

#' @export
plot_roc_points.NULL <- function(data,
                                 response = NULL,
                                 predictor = NULL,
                                 .condition = NULL,
                                 .label = NULL) {
  data <- roc_points(NULL, response, predictor, .condition)
  plot_roc_points.ratio_df(data, response, predictor, .condition, .label)
}

#' @export
plot_roc_points.ratio_df <- function(data,
                                     response = NULL,
                                     predictor = NULL,
                                     .condition = NULL,
                                     .label = NULL) {
  plot_roc_geom(data, geom_point, .label)
}

#' @title Plot a classifier ROC curve
#' @description
#' Create a curve plot using ROC curve points.
#' @inheritParams calc_partial_roc_points
#' @inheritParams plot_roc_points
#' @examples
#' plot_roc_curve(iris, response = Species, predictor = Sepal.Width)
#' @export
plot_roc_curve <- function(data,
                           response = NULL,
                           predictor = NULL,
                           .condition = NULL,
                           .label = NULL) {
  UseMethod("plot_roc_curve", data)
}

#' @export
plot_roc_curve.data.frame <- function(data,
                                      response = NULL,
                                      predictor = NULL,
                                      .condition = NULL,
                                      .label = NULL) {
  if (is.null(.label)) {
    predictor_name <- as_name(enquo(predictor))
  } else {
    predictor_name <- .label
  }

  response <- pull(data, {{ response }})
  predictor <- pull(data, {{ predictor }})

  plot_roc_curve.NULL(NULL, response, predictor, .condition, predictor_name)
}

#' @export
plot_roc_curve.NULL <- function(data,
                                response = NULL,
                                predictor = NULL,
                                .condition = NULL,
                                .label = NULL) {
  data <- roc_points(NULL, response, predictor, .condition)
  plot_roc_curve.ratio_df(data, response, predictor, .condition, .label)
}

#' @export
plot_roc_curve.ratio_df <- function(data,
                                    response = NULL,
                                    predictor = NULL,
                                    .condition = NULL,
                                    .label = NULL) {
  plot_roc_geom(data, geom_path, .label)
}

#' @title Show chance line in a ROC plot
#' @description
#' Plot chance line in a ROC plot.
#' @examples
#' plot_roc_curve(iris, response = Species, predictor = Sepal.Width) +
#'  add_chance_line()
#' @export
add_chance_line <- function() {
  geom_abline(slope = 1, linetype = "dashed", alpha = 1 / 5)
}

#' @title Add a ROC curve plot to an existing one
#' @description
#' Add a ROC curve to an existing ROC plot.
#' @inheritParams plot_roc_curve
#' @inheritParams plot_roc_points
#' @examples
#' plot_roc_curve(iris, response = Species, predictor = Sepal.Width) +
#'  add_roc_curve(iris, response = Species, predictor = Sepal.Length)
#' @export
add_roc_curve <- function(data,
                          response = NULL,
                          predictor = NULL,
                          .condition = NULL,
                          .label = NULL) {
  UseMethod("add_roc_curve", data)
}

#' @export
add_roc_curve.data.frame <- function(data,
                                     response = NULL,
                                     predictor = NULL,
                                     .condition = NULL,
                                     .label = NULL) {
  if (is.null(.label)) {
    predictor_name <- as_name(enquo(predictor))
  } else {
    predictor_name <- .label
  }

  response <- pull(data, {{ response }})
  predictor <- pull(data, {{ predictor }})

  add_roc_curve.NULL(NULL, response, predictor, .condition, predictor_name)
}

#' @export
add_roc_curve.NULL <- function(data,
                               response = NULL,
                               predictor = NULL,
                               .condition = NULL,
                               .label = NULL) {
  data <- roc_points(NULL, response, predictor, .condition)
  add_roc_curve.ratio_df(data, response, predictor, .condition, .label)
}

#' @export
add_roc_curve.ratio_df <- function(data,
                                   response = NULL,
                                   predictor = NULL,
                                   .condition = NULL,
                                   .label = NULL) {
  add_roc_geom(data, geom_path, .label)
}

#' @title Add ROC points plot to an existing one
#' @description
#' Add ROC points to an existing ROC plot.
#' @inheritParams plot_roc_points
#' @examples
#' plot_roc_curve(iris, response = Species, predictor = Sepal.Width) +
#'  add_roc_points(iris, response = Species, predictor = Sepal.Length)
#' @export
add_roc_points <- function(data,
                           response = NULL,
                           predictor = NULL,
                           .condition = NULL,
                           .label = NULL) {
  UseMethod("add_roc_points", data)
}

#' @export
add_roc_points.data.frame <- function(data,
                                      response = NULL,
                                      predictor = NULL,
                                      .condition = NULL,
                                      .label = NULL) {
  if (is.null(.label)) {
    predictor_name <- as_name(enquo(predictor))
  } else {
    predictor_name <- .label
  }

  response <- pull(data, {{ response }})
  predictor <- pull(data, {{ predictor }})

  add_roc_points.NULL(NULL, response, predictor, .condition, predictor_name)
}

#' @export
add_roc_points.NULL <- function(data,
                                response = NULL,
                                predictor = NULL,
                                .condition = NULL,
                                .label = NULL) {
  data <- roc_points(NULL, response, predictor, .condition)
  add_roc_points.ratio_df(data, response, predictor, .condition, .label)
}

#' @export
add_roc_points.ratio_df <- function(data,
                                    response = NULL,
                                    predictor = NULL,
                                    .condition = NULL,
                                    .label = NULL) {
  add_roc_geom(data, geom_point, .label)
}

#' @rdname plot_thresholds
#' @export
add_fpr_threshold_line <- function(threshold) {
  geom_vline(xintercept = threshold, linetype = "dashed")
}

#' @rdname plot_thresholds
#' @export
add_tpr_threshold_line <- function(threshold) {
  geom_hline(yintercept = threshold, linetype = "dashed")
}

#' @title Add a threshold line to a ROC plot
#' @description
#' Include a threshold line on an specified axis.
#' @inheritParams summarize_predictor
#' @param ratio Ratio in which to display threshold.
#' * If `"tpr"` threshold will be displayed in TPR, y axis
#' * If `"fpr"` it will be displayed in FPR, x axis.
#' @name plot_thresholds
#' @examples
#' # Add two threshold line in TPR = 0.9 and FPR = 0.1
#' plot_roc_curve(iris, response = Species, predictor = Sepal.Width) +
#'  add_threshold_line(threshold = 0.9, ratio = "tpr") +
#'  add_threshold_line(threshold = 0.1, ratio = "fpr")
#' # Add threshold line in TPR = 0.9
#' plot_roc_curve(iris, response = Species, predictor = Sepal.Width) +
#'  add_tpr_threshold_line(threshold = 0.9)
#' # Add threshold line in FPR = 0.1
#' plot_roc_curve(iris, response = Species, predictor = Sepal.Width) +
#'  add_fpr_threshold_line(threshold = 0.1)
#' @export
add_threshold_line <- function(threshold,
                               ratio = NULL) {
  if (ratio == "fpr") {
    add_fpr_threshold_line(threshold)
  } else if (ratio == "tpr") {
    add_tpr_threshold_line(threshold)
  }
}

#' @title Add a section of a ROC curve to an existing one
#' @description
#' Add an specific region of a ROC curve to an existing ROC plot.
#' @inheritParams plot_partial_roc_curve
#' @inheritParams plot_roc_points
#' @examples
#' plot_roc_curve(iris, response = Species, predictor = Sepal.Width) +
#'   add_partial_roc_curve(
#'     iris,
#'     response = Species,
#'     predictor = Sepal.Length,
#'     ratio = "tpr",
#'     threshold = 0.9
#'   )
#' @export
add_partial_roc_curve <- function(data,
                                  response = NULL,
                                  predictor = NULL,
                                  ratio,
                                  threshold,
                                  .condition = NULL,
                                  .label = NULL) {
  UseMethod("add_partial_roc_curve", data)
}

#' @export
add_partial_roc_curve.data.frame <- function(data,
                                             response = NULL,
                                             predictor = NULL,
                                             ratio,
                                             threshold,
                                             .condition = NULL,
                                             .label = NULL) {
  if (is.null(.label)) {
    predictor_name <- as_name(enquo(predictor))
  } else {
    predictor_name <- .label
  }

  response <- pull(data, {{ response }})
  predictor <- pull(data, {{ predictor }})

  add_partial_roc_curve.NULL(
    data = data,
    response = response,
    predictor = predictor,
    ratio = ratio,
    threshold = threshold,
    .condition = .condition,
    .label = predictor_name
  )
}

#' @export
add_partial_roc_curve.NULL <- function(data,
                                       response = NULL,
                                       predictor = NULL,
                                       ratio,
                                       threshold,
                                       .condition = NULL,
                                       .label = NULL) {
  data <- roc_points(NULL, response, predictor, .condition)
  add_partial_roc_curve.ratio_df(
    data = data,
    response = response,
    predictor = predictor,
    ratio = ratio,
    threshold = threshold,
    .condition = .condition,
    .label = .label
  )
}

#' @export
add_partial_roc_curve.ratio_df <- function(data,
                                           response = NULL,
                                           predictor = NULL,
                                           ratio,
                                           threshold,
                                           .condition = NULL,
                                           .label = NULL) {
  if (ratio == "tpr") {
    data <- filter(data, .data[["tpr"]] >= threshold)
  } else if (ratio == "fpr") {
    data <- filter(data, .data[["fpr"]] <= threshold)
  }
  add_roc_geom(data, geom_path, .label)
}

#' @title Add points in a section of a ROC curve to an existing plot
#' @description
#' Add points in a specific ROC region to an existing ROC plot.
#' @inheritParams plot_partial_roc_points
#' @inheritParams plot_roc_points
#' @examples
#' plot_roc_curve(iris, response = Species, predictor = Sepal.Width) +
#'   add_partial_roc_points(
#'     iris,
#'     response = Species,
#'     predictor = Sepal.Length,
#'     ratio = "tpr",
#'     threshold = 0.9
#'   )
#' @export
add_partial_roc_points <- function(data,
                                   response = NULL,
                                   predictor = NULL,
                                   ratio,
                                   threshold,
                                   .condition = NULL,
                                   .label = NULL) {
  UseMethod("add_partial_roc_points", data)
}

#' @export
add_partial_roc_points.data.frame <- function(data,
                                              response = NULL,
                                              predictor = NULL,
                                              ratio,
                                              threshold,
                                              .condition = NULL,
                                              .label = NULL) {
  if (is.null(.label)) {
    predictor_name <- as_name(enquo(predictor))
  } else {
    predictor_name <- .label
  }

  response <- pull(data, {{ response }})
  predictor <- pull(data, {{ predictor }})

  add_partial_roc_points.NULL(
    data = data,
    response = response,
    predictor = predictor,
    ratio = ratio,
    threshold = threshold,
    .condition = .condition,
    .label = predictor_name
  )
}

#' @export
add_partial_roc_points.NULL <- function(data,
                                        response = NULL,
                                        predictor = NULL,
                                        ratio,
                                        threshold,
                                        .condition = NULL,
                                        .label = NULL) {
  data <- roc_points(NULL, response, predictor, .condition)
  add_partial_roc_points.ratio_df(
    data = data,
    response = response,
    predictor = predictor,
    ratio = ratio,
    threshold = threshold,
    .condition = .condition,
    .label = .label
  )
}

#' @export
add_partial_roc_points.ratio_df <- function(data,
                                            response = NULL,
                                            predictor = NULL,
                                            ratio,
                                            threshold,
                                            .condition = NULL,
                                            .label = NULL) {
  if (ratio == "tpr") {
    data <- filter(data, .data[["tpr"]] >= threshold)
  } else if (ratio == "fpr") {
    data <- filter(data, .data[["fpr"]] <= threshold)
  }

  add_roc_geom(data, geom_point, .label)
}


#' @title Plot a section of a classifier ROC curve
#' @description
#' Create a curve plot using points in an specific region of ROC curve.
#' @inheritParams plot_partial_roc_points
#' @inheritParams plot_roc_points
#' @examples
#' plot_partial_roc_curve(
#'  iris,
#'  response = Species,
#'  predictor = Sepal.Width,
#'  ratio = "tpr",
#'  threshold = 0.9
#' )
#' @export
plot_partial_roc_curve <- function(data,
                                   response = NULL,
                                   predictor = NULL,
                                   ratio,
                                   threshold,
                                   .condition = NULL,
                                   .label = NULL) {
  plot_roc(data) +
    add_partial_roc_curve(
      data = data,
      response = {{ response }},
      predictor = {{ predictor }},
      ratio = ratio,
      threshold = threshold,
      .condition = .condition,
      .label = .label
    )
}


#' @title Plot points in a region of a ROC curve
#' @description
#' Create an scatter plot using points in an specific region of ROC curve.
#' @inheritParams calc_partial_roc_points
#' @inheritParams summarize_predictor
#' @inheritParams plot_roc_points
#' @examples
#' plot_partial_roc_points(
#'  iris,
#'  response = Species,
#'  predictor = Sepal.Width,
#'  ratio = "tpr",
#'  threshold = 0.9
#' )
#' @export
plot_partial_roc_points <- function(data,
                                    response = NULL,
                                    predictor = NULL,
                                    ratio,
                                    threshold,
                                    .condition = NULL,
                                    .label = NULL) {
  plot_roc(data) +
    add_partial_roc_points(
      data = data,
      response = {{ response }},
      predictor = {{ predictor }},
      ratio = ratio,
      threshold = threshold,
      .condition = .condition,
      .label = .label
    )
}


#' @rdname fpauc_lower_bounds
#' @export
add_fpauc_partially_proper_lower_bound <- function(data,
                                                   response = NULL,
                                                   predictor = NULL,
                                                   threshold,
                                                   .condition = NULL,
                                                   .label = NULL) {
  UseMethod("add_fpauc_partially_proper_lower_bound", data)
}

#' @export
add_fpauc_partially_proper_lower_bound.data.frame <- function(data,
                                                              response = NULL,
                                                              predictor = NULL,
                                                              threshold,
                                                              .condition = NULL,
                                                              .label = NULL) {
  if (is.null(.label)) {
    predictor_name <- as_name(enquo(predictor))
  } else {
    predictor_name <- .label
  }

  response <- pull(data, {{ response }})
  predictor <- pull(data, {{ predictor }})

  add_fpauc_partially_proper_lower_bound.NULL(
    data = data,
    response = response,
    predictor = predictor,
    threshold = threshold,
    .condition = .condition,
    .label = predictor_name
  )
}

#' @export
add_fpauc_partially_proper_lower_bound.NULL <- function(data,
                                                        response = NULL,
                                                        predictor = NULL,
                                                        threshold,
                                                        .condition = NULL,
                                                        .label = NULL) {
  roc_points <- roc_points(
    data = NULL,
    response = response,
    predictor = predictor,
    .condition = .condition
  )

  add_fpauc_partially_proper_lower_bound.ratio_df(
    data = roc_points,
    response = response,
    predictor = predictor,
    threshold = threshold,
    .condition = .condition,
    .label = .label
  )
}

#' @export
add_fpauc_partially_proper_lower_bound.ratio_df <- function(data,
                                                            response = NULL,
                                                            predictor = NULL,
                                                            threshold,
                                                            .condition = NULL,
                                                            .label = NULL) {
  if (!is.null(.label)) {
    fill <- str_c(.label, " FpAUC lower bound")
  } else {
    fill <- NULL
  }

  geom_polygon(
    data = tibble(
      x = c(threshold, 1, 1),
      y = c(threshold, 1, threshold)
    ),
    mapping = aes(
      .data$x,
      .data$y,
      color = .label,
      fill = fill
    ),
    alpha = 1 / 5,
    linetype = "solid"
  )
}

#' @rdname fpauc_lower_bounds
#' @export
add_fpauc_concave_lower_bound <- function(data,
                                          response = NULL,
                                          predictor = NULL,
                                          threshold,
                                          .condition = NULL,
                                          .label = NULL) {
  UseMethod("add_fpauc_concave_lower_bound", data)
}

#' @export
add_fpauc_concave_lower_bound.data.frame <- function(data,
                                                     response = NULL,
                                                     predictor = NULL,
                                                     threshold,
                                                     .condition = NULL,
                                                     .label = NULL) {

  if (is.null(.label)) {
    predictor_name <- as_name(enquo(predictor))
  } else {
    predictor_name <- .label
  }

  response <- pull(data, {{ response }})
  predictor <- pull(data, {{ predictor }})

  add_fpauc_concave_lower_bound.NULL(
    data = data,
    response = response,
    predictor = predictor,
    threshold = threshold,
    .condition = .condition,
    .label = predictor_name
  )
}

#' @export
add_fpauc_concave_lower_bound.NULL <- function(data,
                                               response = NULL,
                                               predictor = NULL,
                                               threshold,
                                               .condition = NULL,
                                               .label = NULL) {
  partial_points <- calc_partial_roc_points(
    data = NULL,
    response = response,
    predictor = predictor,
    lower_threshold = threshold,
    upper_threshold = 1,
    ratio = "tpr",
    .condition = .condition
  )

  add_fpauc_concave_lower_bound.ratio_df(
    data = partial_points,
    response = response,
    predictor = predictor,
    threshold = threshold,
    .condition = .condition,
    .label = .label
  )
}

#' @export
add_fpauc_concave_lower_bound.ratio_df <- function(data,
                                                   response = NULL,
                                                   predictor = NULL,
                                                   threshold,
                                                   .condition = NULL,
                                                   .label = NULL) {
  if (!is.null(.label)) {
    fill <- str_c(.label, " FpAUC lower bound")
  } else {
    fill <- NULL
  }

  threshold_fpr <- data[["fpr"]][1]

  geom_polygon(
    data = tibble(
      x = c(threshold_fpr, 1, 1),
      y = c(threshold, threshold, 1)
    ),
    mapping = aes(
      .data$x,
      .data$y,
      color = .label,
      fill = fill
    ),
    alpha = 1 / 5,
    linetype = "solid"
  )
}

#' @title Add FpAUC lower bound to a ROC plot
#' @description
#' Calculate and plot lower bound defined by FpAUC sensitivity index.
#'
#' * `add_fpauc_lower_bound()` provides an upper level function which
#' automatically calculates curve shape and plots a lower bound that better fits
#' it.
#' * `add_fpauc_partially_proper_lower_bound()` and
#' `add_fpauc_concave_lower_bound()` are lower level functions that enforce the
#' plot of specific bounds.
#'
#' First one plots lower bound when curve shape is
#' partially proper (presents some kind of hook). Second one plots lower bound
#' when curve shape is concave in the region of interest.
#' @inheritParams calc_partial_roc_points
#' @inheritParams plot_roc_points
#' @param threshold A number between 0 and 1, inclusive. This number represents
#' the lower value of TPR for the region where to calculate and plot
#' lower bound.
#'
#' Because of definition of [fp_auc()], region upper bound will be established
#' as 1.
#' @name fpauc_lower_bounds
#' @examples
#' # Add lower bound based on curve shape (Concave)
#' plot_roc_curve(iris, response = Species, predictor = Sepal.Width) +
#'   add_fpauc_lower_bound(
#'     data = iris,
#'     response = Species,
#'     predictor = Sepal.Width,
#'     threshold = 0.9
#'   )
#' @export
add_fpauc_lower_bound <- function(data,
                                  response = NULL,
                                  predictor = NULL,
                                  threshold,
                                  .condition = NULL,
                                  .label = NULL) {
  curve_shape <- data %>%
    calc_curve_shape(
      response = {{ response }},
      predictor = {{ predictor }},
      lower_threshold = threshold,
      upper_threshold = 1,
      ratio = "tpr",
      .condition = .condition
    )
  if (curve_shape == "Concave") {
    bound <- add_fpauc_concave_lower_bound(
      data,
      response = {{ response }},
      predictor = {{ predictor }},
      threshold = threshold,
      .condition = .condition,
      .label = .label
    )
  } else if (curve_shape == "Partially proper") {
    bound <- add_fpauc_partially_proper_lower_bound(
      data,
      response = {{ response }},
      predictor = {{ predictor }},
      threshold = threshold,
      .condition = .condition,
      .label = .label
    )
  } else if (curve_shape == "Hook under chance") {
    bound <- NULL
  }
  bound
}

#' @rdname tpauc_lower_bounds
#' @export
add_tpauc_concave_lower_bound <- function(data,
                                          response = NULL,
                                          predictor = NULL,
                                          lower_threshold,
                                          upper_threshold,
                                          .condition = NULL,
                                          .label = NULL) {
  UseMethod("add_tpauc_concave_lower_bound", data)
}

#' @export
add_tpauc_concave_lower_bound.data.frame <- function(data,
                                                     response = NULL,
                                                     predictor = NULL,
                                                     lower_threshold,
                                                     upper_threshold,
                                                     .condition = NULL,
                                                     .label = NULL) {
  if (is.null(.label)) {
    predictor_name <- as_name(enquo(predictor))
  } else {
    predictor_name <- .label
  }

  predictor <- pull(data, {{ predictor }})
  response <- pull(data, {{ response }})

  add_tpauc_concave_lower_bound.NULL(
    data = NULL,
    predictor = predictor,
    response = response,
    lower_threshold = lower_threshold,
    upper_threshold = upper_threshold,
    .condition = .condition,
    .label = predictor_name
  )
}

#' @export
add_tpauc_concave_lower_bound.NULL <- function(data,
                                               response = NULL,
                                               predictor = NULL,
                                               lower_threshold,
                                               upper_threshold,
                                               .condition = NULL,
                                               .label = NULL) {
  ppoints <- calc_partial_roc_points(
    data = NULL,
    response = response,
    predictor = predictor,
    lower_threshold = lower_threshold,
    upper_threshold = upper_threshold,
    ratio = "fpr",
    .condition = .condition
  )

  add_tpauc_concave_lower_bound.ratio_df(
    data = ppoints,
    response = response,
    predictor = predictor,
    lower_threshold = lower_threshold,
    upper_threshold = upper_threshold,
    .condition = .condition,
    .label = .label
  )
}

#' @export
add_tpauc_concave_lower_bound.ratio_df <- function(data,
                                                   response = NULL,
                                                   predictor = NULL,
                                                   lower_threshold,
                                                   upper_threshold,
                                                   .condition = NULL,
                                                   .label = NULL) {
  if (!is.null(.label)) {
    fill <- str_c(.label, " TpAUC lower bound")
  } else {
    fill <- NULL
  }

  lower_threshold_tpr <- data %>%
    filter(.data$fpr == lower_threshold) %>%
    slice_min(.data$tpr, n = 1, with_ties = FALSE) %>%
    pull(.data$tpr)
  upper_threshold_tpr <- data %>%
    filter(.data$fpr == upper_threshold) %>%
    slice_max(.data$tpr, n = 1, with_ties = FALSE) %>%
    pull(.data$tpr)

  geom_polygon(
    data = tibble(
      x = c(
        lower_threshold,
        upper_threshold,
        upper_threshold,
        lower_threshold
      ),
      y = c(0, 0, upper_threshold_tpr, lower_threshold_tpr)
    ),
    mapping = aes(
      .data$x,
      .data$y,
      color = .label,
      fill = fill
    ),
    alpha = 1 / 5,
    linetype = "solid"
  )
}

#' @rdname tpauc_lower_bounds
#' @export
add_tpauc_partially_proper_lower_bound <- function(data,
                                                   response = NULL,
                                                   predictor = NULL,
                                                   lower_threshold,
                                                   upper_threshold,
                                                   .condition = NULL,
                                                   .label = NULL) {
  UseMethod("add_tpauc_partially_proper_lower_bound", data)
}


#' @export
add_tpauc_partially_proper_lower_bound.data.frame <- function(data,
                                                              response = NULL,
                                                              predictor = NULL,
                                                              lower_threshold,
                                                              upper_threshold,
                                                              .condition = NULL,
                                                              .label = NULL) {
  if (is.null(.label)) {
    predictor_name <- as_name(enquo(predictor))
  } else {
    predictor_name <- .label
  }

  predictor <- pull(data, {{ predictor }})
  response <- pull(data, {{ response }})

  add_tpauc_partially_proper_lower_bound.NULL(
    data = NULL,
    predictor = predictor,
    response = response,
    lower_threshold = lower_threshold,
    upper_threshold = upper_threshold,
    .condition = .condition,
    .label = predictor_name
  )
}

#' @export
add_tpauc_partially_proper_lower_bound.NULL <- function(data,
                                                        response = NULL,
                                                        predictor = NULL,
                                                        lower_threshold,
                                                        upper_threshold,
                                                        .condition = NULL,
                                                        .label = NULL) {
  partial_points <- calc_partial_roc_points(
    data = NULL,
    response = response,
    predictor = predictor,
    lower_threshold = lower_threshold,
    upper_threshold = upper_threshold,
    ratio = "fpr",
    .condition = .condition
  )

  add_tpauc_partially_proper_lower_bound.ratio_df(
    data = partial_points,
    response = response,
    predictor = predictor,
    lower_threshold = lower_threshold,
    upper_threshold = upper_threshold,
    .condition = .condition,
    .label = .label
  )
}

#' @export
add_tpauc_partially_proper_lower_bound.ratio_df <- function(data,
                                                            response = NULL,
                                                            predictor = NULL,
                                                            lower_threshold,
                                                            upper_threshold,
                                                            .condition = NULL,
                                                            .label = NULL) {
  if (!is.null(.label)) {
    fill <- str_c(.label, " TpAUC lower bound")
  } else {
    fill <- NULL
  }

  lower_threshold_tpr <- data %>%
    filter(.data$fpr == lower_threshold) %>%
    slice_min(.data$tpr, n = 1, with_ties = FALSE) %>%
    pull(.data$tpr)

  diagonal_area <- calc_fpr_diagonal_lower_bound(
    data[["fpr"]],
    data[["tpr"]]
  )
  square_area <- calc_fpr_square_lower_bound(
    data[["fpr"]],
    data[["tpr"]]
  )

  if (diagonal_area > square_area) {
    bound_points <- tibble(
      x = c(
        lower_threshold,
        upper_threshold,
        upper_threshold,
        lower_threshold
      ),
      y = c(0, 0, upper_threshold, lower_threshold)
    )
  } else if (diagonal_area > square_area) {
    bound_points <- tibble(
      x = c(
        lower_threshold,
        upper_threshold,
        upper_threshold,
        lower_threshold
      ),
      y = c(0, 0, lower_threshold_tpr, lower_threshold_tpr)
    )
  }

  geom_polygon(
    data = bound_points,
    mapping = aes(
      .data$x,
      .data$y,
      color = .label,
      fill = fill
    ),
    alpha = 1 / 5,
    linetype = "solid"
  )
}

#' @rdname tpauc_lower_bounds
#' @export
add_tpauc_under_chance_lower_bound <- function(data,
                                               response = NULL,
                                               predictor = NULL,
                                               lower_threshold,
                                               upper_threshold,
                                               .condition = NULL,
                                               .label = NULL) {
  UseMethod("add_tpauc_under_chance_lower_bound", data)
}

#' @export
add_tpauc_under_chance_lower_bound.data.frame <- function(data,
                                                          response = NULL,
                                                          predictor = NULL,
                                                          lower_threshold,
                                                          upper_threshold,
                                                          .condition = NULL,
                                                          .label = NULL) {
  if (is.null(.label)) {
    predictor_name <- as_name(enquo(predictor))
  } else {
    predictor_name <- .label
  }

  predictor <- pull(data, {{ predictor }})
  response <- pull(data, {{ response }})

  add_tpauc_under_chance_lower_bound.NULL(
    data = NULL,
    predictor = predictor,
    response = response,
    lower_threshold = lower_threshold,
    upper_threshold = upper_threshold,
    .condition = .condition,
    .label = predictor_name
  )
}

#' @export
add_tpauc_under_chance_lower_bound.NULL <- function(data,
                                                    response = NULL,
                                                    predictor = NULL,
                                                    lower_threshold,
                                                    upper_threshold,
                                                    .condition = NULL,
                                                    .label = NULL) {
  partial_points <- calc_partial_roc_points(
    data = NULL,
    response = response,
    predictor = predictor,
    lower_threshold = lower_threshold,
    upper_threshold = upper_threshold,
    ratio = "fpr",
    .condition = .condition
  )

  add_tpauc_under_chance_lower_bound.ratio_df(
    data = partial_points,
    response = response,
    predictor = predictor,
    lower_threshold = lower_threshold,
    upper_threshold = upper_threshold,
    .condition = .condition,
    .label = .label
  )
}

#' @export
add_tpauc_under_chance_lower_bound.ratio_df <- function(data,
                                                        response = NULL,
                                                        predictor = NULL,
                                                        lower_threshold,
                                                        upper_threshold,
                                                        .condition = NULL,
                                                        .label = NULL) {
  if (!is.null(.label)) {
    fill <- str_c(.label, " TpAUC lower bound")
  } else {
    fill <- NULL
  }

  lower_threshold_tpr <- data %>%
    filter(.data$fpr == lower_threshold) %>%
    slice_min(.data$tpr, n = 1, with_ties = FALSE) %>%
    pull(.data$tpr)

  geom_polygon(
    data = tibble(
      x = c(
        lower_threshold,
        upper_threshold,
        upper_threshold,
        lower_threshold
      ),
      y = c(0, 0, lower_threshold_tpr, lower_threshold_tpr)
    ),
    mapping = aes(
      .data$x,
      .data$y,
      color = .label,
      fill = fill
    ),
    alpha = 1 / 5,
    linetype = "solid"
  )
}

#' @title Add TpAUC lower bound to a ROC plot
#' @description
#' Calculate and plot lower bound defined by TpAUC specificity index.
#'
#' * `add_tpauc_lower_bound()` provides a upper level function which
#' automatically calculates curve shape and plots a lower bound that better fits
#' it.
#'
#' Additionally, several lower level functions are provided to plot
#' specific lower bounds:
#' * `add_tpauc_concave_lower_bound()`. Plot lower bound corresponding to a ROC
#' curve with concave shape in selected region.
#' * `add_tpauc_partially_proper_lower_bound`. Plot lower bound corresponding to
#' a ROC curve with partially proper (presence of some hook) in
#' selected region.
#' * `add_tpauc_under_chance_lower_bound`. Plot lower bound corresponding to
#' a ROC curve with a hook under chance line in selected region.
#' @inheritParams calc_partial_roc_points
#' @inheritParams plot_roc_points
#' @param lower_threshold,upper_threshold Two numbers between 0 and 1,
#' inclusive. These numbers represent lower and upper values of FPR region where
#' to calculate and plot lower bound.
#' @name tpauc_lower_bounds
#' @examples
#' plot_roc_curve(iris, response = Species, predictor = Sepal.Width) +
#'   add_tpauc_lower_bound(
#'     data = iris,
#'     response = Species,
#'     predictor = Sepal.Width,
#'     upper_threshold =  0.1,
#'     lower_threshold = 0
#'   )
#' @export
add_tpauc_lower_bound <- function(data,
                                  response = NULL,
                                  predictor = NULL,
                                  lower_threshold,
                                  upper_threshold,
                                  .condition = NULL,
                                  .label = NULL) {
  curve_shape <- data %>%
    calc_curve_shape(
      response = {{ response }},
      predictor = {{ predictor }},
      lower_threshold = lower_threshold,
      upper_threshold = upper_threshold,
      ratio = "fpr",
      .condition = .condition
    )
  if (curve_shape == "Concave") {
    bound <- add_tpauc_concave_lower_bound(
      data = data,
      response = {{ response }},
      predictor = {{ predictor }},
      upper_threshold = upper_threshold,
      lower_threshold = lower_threshold,
      .condition = .condition,
      .label = .label
    )
  } else if (curve_shape == "Partially Proper") {
    bound <- add_tpauc_partially_proper_lower_bound(
      data = data,
      response = {{ response }},
      predictor = {{ predictor }},
      upper_threshold = upper_threshold,
      lower_threshold = lower_threshold,
      .condition = .condition,
      .label = .label
    )
  } else if (curve_shape == "Hook under chance") {
    bound <- add_tpauc_under_chance_lower_bound(
      data = data,
      response = {{ response }},
      predictor = {{ predictor }},
      upper_threshold = upper_threshold,
      lower_threshold = lower_threshold,
      .condition = .condition,
      .label = .label
    )
  }
  bound
}

#' @title Add NpAUC lower bound to a ROC plot
#' @description
#' Calculate and plot lower bound defined by NpAUC specificity index.
#'
#' * `add_npauc_normalized_lower_bound()` allows to plot normalized
#' lower bound, which is used to formally calculate NpAUC.
#'
#' * `add_npauc_lower_bound()` is a lower level function
#' providing a way to plot lower bound previous to normalization.
#' @inheritParams calc_partial_roc_points
#' @inheritParams plot_roc_points
#' @param threshold A number between 0 and 1, inclusive. This number represents
#' the lower value of TPR for the region where to calculate and plot
#' lower bound.
#'
#' Because of definition of [np_auc()], region upper bound will be established
#' as 1.
#' @name npauc_lower_bounds
#' @examples
#' plot_roc_curve(iris, response = Species, predictor = Sepal.Width) +
#'   add_npauc_lower_bound(
#'     iris,
#'     response = Species,
#'     predictor = Sepal.Width,
#'     threshold = 0.9
#'   )
#' @export
add_npauc_lower_bound <- function(data,
                                  response = NULL,
                                  predictor = NULL,
                                  threshold,
                                  .condition = NULL,
                                  .label = NULL) {
  UseMethod("add_npauc_lower_bound", data)
}


#' @export
add_npauc_lower_bound.data.frame <- function(data,
                                             response = NULL,
                                             predictor = NULL,
                                             threshold,
                                             .condition = NULL,
                                             .label = NULL) {
  if (is.null(.label)) {
    predictor_name <- as_name(enquo(predictor))
  } else {
    predictor_name <- .label
  }

  predictor <- pull(data, {{ predictor }})
  response <- pull(data, {{ response }})

  add_npauc_lower_bound.NULL(
    data = NULL,
    predictor = predictor,
    response = response,
    threshold = threshold,
    .condition = .condition,
    .label = predictor_name
  )
}

#' @export
add_npauc_lower_bound.NULL <- function(data,
                                       response = NULL,
                                       predictor = NULL,
                                       threshold,
                                       .condition = NULL,
                                       .label = NULL) {
  roc_points <- roc_points(
    data = NULL,
    response = response,
    predictor = predictor,
  )

  add_npauc_lower_bound.ratio_df(
    data = roc_points,
    response = response,
    predictor = predictor,
    threshold = threshold,
    .condition = .condition,
    .label = .label
  )
}

#' @export
add_npauc_lower_bound.ratio_df <- function(data,
                                           response = NULL,
                                           predictor = NULL,
                                           threshold,
                                           .condition = NULL,
                                           .label = NULL) {
  if (!is.null(.label)) {
    fill <- str_c(.label, " NpAUC lower bound")
  } else {
    fill <- NULL
  }

  geom_polygon(
    data = tibble(
      x = c(threshold, 1, 1),
      y = c(threshold, 1, threshold)
    ),
    mapping = aes(
      .data$x,
      .data$y,
      color = .label,
      fill = fill
    ),
    alpha = 1 / 5,
    linetype = "solid"
  )
}

#' @rdname npauc_lower_bounds
#' @export
add_npauc_normalized_lower_bound <- function(data,
                                             response = NULL,
                                             predictor = NULL,
                                             threshold,
                                             .condition = NULL,
                                             .label = NULL) {
  UseMethod("add_npauc_normalized_lower_bound", data)
}

#' @export
add_npauc_normalized_lower_bound.data.frame <- function(data,
                                                        response = NULL,
                                                        predictor = NULL,
                                                        threshold,
                                                        .condition = NULL,
                                                        .label = NULL) {

  if (is.null(.label)) {
    predictor_name <- as_name(enquo(predictor))
  } else {
    predictor_name <- .label
  }

  predictor <- pull(data, {{ predictor }})
  response <- pull(data, {{ response }})

  add_npauc_normalized_lower_bound.NULL(
    data = NULL,
    predictor = predictor,
    response = response,
    threshold = threshold,
    .condition = .condition,
    .label = predictor_name
  )
}

#' @export
add_npauc_normalized_lower_bound.NULL <- function(data,
                                                  response = NULL,
                                                  predictor = NULL,
                                                  threshold,
                                                  .condition = NULL,
                                                  .label = NULL) {
  roc_points <- roc_points(
    data = NULL,
    response = response,
    predictor = predictor,
    .condition = .condition
  )

  add_npauc_normalized_lower_bound.ratio_df(
    data = roc_points,
    response = response,
    predictor = predictor,
    threshold = threshold,
    .condition = .condition,
    .label = .label
  )
}

#' @export
add_npauc_normalized_lower_bound.ratio_df <- function(data,
                                                      response = NULL,
                                                      predictor = NULL,
                                                      threshold,
                                                      .condition = NULL,
                                                      .label = NULL) {
  if (!is.null(.label)) {
    fill <- str_c(.label, " NpAUC lower bound")
  } else {
    fill <- NULL
  }

  geom_polygon(
    data = tibble(
      x = c(0, 1, 1),
      y = c(threshold, 1, threshold)
    ),
    mapping = aes(
      .data$x,
      .data$y,
      color = .label,
      fill = fill
    ),
    alpha = 1 / 5,
    linetype = "solid"
  )
}

#' @title Add SpAUC lower bound to a ROC plot
#' @description
#' Calculate and plot lower bound defined by SpAUC specificity index.
#' @details
#' SpAUC presents some limitations regarding its lower bound. Lower bound
#' defined by this index cannot be applied to sections where ROC curve is
#' defined under chance line.
#'
#' `add_spauc_lower_bound()` doesn't make any check to ensure the index can be
#' safely applied. Consequently, it allows to enforce the representation even
#' though SpAUC cound't be calculated in the region.
#' @inheritParams calc_partial_roc_points
#' @inheritParams plot_roc_points
#' @name spauc_lower_bounds
#' @examples
#' plot_roc_curve(iris, response = Species, predictor = Sepal.Width) +
#'   add_spauc_lower_bound(
#'     iris,
#'     response = Species,
#'     predictor = Sepal.Width,
#'     lower_threshold = 0,
#'     upper_threshold = 0.1
#'   )
#' @export
add_spauc_lower_bound <- function(data,
                                  response = NULL,
                                  predictor = NULL,
                                  lower_threshold,
                                  upper_threshold,
                                  .condition = NULL,
                                  .label = NULL) {
  UseMethod("add_spauc_lower_bound", data)
}

#' @export
add_spauc_lower_bound.data.frame <- function(data,
                                             response = NULL,
                                             predictor = NULL,
                                             lower_threshold,
                                             upper_threshold,
                                             .condition = NULL,
                                             .label = NULL) {
  if (is.null(.label)) {
    predictor_name <- as_name(enquo(predictor))
  } else {
    predictor_name <- .label
  }

  predictor <- pull(data, {{ predictor }})
  response <- pull(data, {{ response }})

  add_spauc_lower_bound.NULL(
    data = NULL,
    predictor = predictor,
    response = response,
    lower_threshold = lower_threshold,
    upper_threshold = upper_threshold,
    .condition = .condition,
    .label = predictor_name
  )
}

#' @export
add_spauc_lower_bound.NULL <- function(data,
                                       response = NULL,
                                       predictor = NULL,
                                       lower_threshold,
                                       upper_threshold,
                                       .condition = NULL,
                                       .label = NULL) {
  roc_points <- roc_points(
    data = NULL,
    response = response,
    predictor = predictor,
    .condition = .condition
  )

  add_spauc_lower_bound.ratio_df(
    data = roc_points,
    response = response,
    predictor = predictor,
    lower_threshold = lower_threshold,
    upper_threshold = upper_threshold,
    .condition = .condition,
    .label = .label
  )
}

#' @export
add_spauc_lower_bound.ratio_df <- function(data,
                                           response = NULL,
                                           predictor = NULL,
                                           lower_threshold,
                                           upper_threshold,
                                           .condition = NULL,
                                           .label = NULL) {

  if (!is.null(.label)) {
    fill <- str_c(.label, " SpAUC lower bound")
  } else {
    fill <- NULL
  }


  geom_polygon(
    data = tibble(
      x = c(
        lower_threshold,
        upper_threshold,
        upper_threshold,
        lower_threshold
      ),
      y = c(
        0, 0, upper_threshold, lower_threshold
      )
    ),
    mapping = aes(
      .data$x,
      .data$y,
      color = .label,
      fill = fill
    ),
    alpha = 1 / 5,
    linetype = "solid"
  )
}

#' @title Hide legend in a ROC plot
#' @description Hide legend showing name of ploted classifiers and bounds in a
#' ROC curve plot.
#' @export
hide_legend <- function() {
  theme(legend.position = "none")
}
