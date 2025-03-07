plot_roc <- function(data) {
  ggplot(data) +
    labs(x = "FPR", y = "TPR", color = "Predictor", fill = "Bound")
}

#' @title Plot classifier points of a ROC curve
#' @description
#' Create an scatter plot using ROC curve points.
#' @inheritParams calc_partial_roc_points
#' @examples
#' plot_roc_points(iris, response = Species, predictor = Sepal.Width)
#' @export
plot_roc_points <- function(data,
                            fpr = NULL,
                            tpr = NULL,
                            response = NULL,
                            predictor = NULL,
                            .condition = NULL) {
  predictor_expr <- enquo(predictor)
  response_expr <- enquo(response)
  if (!quo_is_null(predictor_expr) && !quo_is_null(response_expr)) {
    predictor_name <- as_name(enquo(predictor))
    plot_roc(data) +
      geom_point(
        data = data %>% roc_points({{ response }}, {{ predictor }}, .condition),
        mapping = aes(
          x = .data[["fpr"]],
          y = .data[["tpr"]],
          color = predictor_name
        ),
        size = 0.8
      )
  } else {
    plot_roc(data) +
      geom_point(
        mapping = aes(
          x = {{ fpr }},
          y = {{ tpr }}
        ),
        size = 0.8
      )
  }
}

#' @title Plot a classifier ROC curve
#' @description
#' Create a curve plot using ROC curve points.
#' @inheritParams calc_partial_roc_points
#' @examples
#' plot_roc_curve(iris, response = Species, predictor = Sepal.Width)
#' @export
plot_roc_curve <- function(data,
                           fpr = NULL,
                           tpr = NULL,
                           response = NULL,
                           predictor = NULL,
                           .condition = NULL) {
  predictor_expr <- enquo(predictor)
  response_expr <- enquo(response)
  if (!quo_is_null(predictor_expr) && !quo_is_null(response_expr)) {
    if (quo_is_symbol(predictor_expr)) {
      color <- as_name({{ predictor_expr }})
    } else if (quo_is_call(predictor_expr)) {
      expr_args <- rlang::call_args(
        rlang::get_expr(predictor_expr)
      )
      if (expr_args[[1]] == ".data" || expr_args[[1]] == ".env") {
        color <- expr_args[[2]]
      } else {
        color <- NULL
      }
    }
    plot_roc(data) +
      geom_path(
        data = data %>% roc_points({{ response }}, {{ predictor }}, .condition),
        mapping = aes(
          x = .data[["fpr"]],
          y = .data[["tpr"]],
          color = color
        ),
        size = 0.8
      )
  } else {
    plot_roc(data) +
      geom_path(
        mapping = aes(
          x = {{ fpr }},
          y = {{ tpr }}
        ),
        size = 0.8
      )
  }
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

add_roc_from_predictor <- function(data,
                                   response,
                                   predictor,
                                   geom = NULL,
                                   .condition = NULL) {
  predictor_expr <- enquo(predictor)
  predictor_name <- mask_name(predictor_expr)
  if (is.null(data)) {
    geom(
      data = . %>% roc_points({{ response }}, {{ predictor }}, .condition),
      mapping = aes(
        x = .data[["fpr"]],
        y = .data[["tpr"]],
        color = predictor_name
      ),
      size = 0.8
    )
  } else {
    geom(
      data = data %>% roc_points({{ response }}, {{ predictor }}, .condition),
      mapping = aes(
        x = .data[["fpr"]],
        y = .data[["tpr"]],
        color = predictor_name
      ),
      size = 0.8
    )
  }
}

add_roc_from_ratios <- function(data,
                                fpr = fpr,
                                tpr = tpr,
                                geom = NULL) {
  geom(
    data = data,
    mapping = aes(
      x = {{ fpr }},
      y = {{ tpr }}
    ),
    size = 0.8
  )
}

add_roc_curve_from_predictor <- function(data,
                                         response,
                                         predictor,
                                         .condition = NULL) {
  add_roc_from_predictor(
    data,
    {{ response }},
    {{ predictor }},
    geom_path,
    .condition
  )
}

add_roc_curve_from_ratios <- function(data,
                                      fpr,
                                      tpr) {
  add_roc_from_ratios(data, {{ fpr }}, {{ tpr }}, geom_path)
}

add_roc_points_from_predictor <- function(data,
                                          response,
                                          predictor,
                                          .condition = NULL) {
  add_roc_from_predictor(
    data,
    {{ response }},
    {{ predictor }},
    geom_point,
    .condition
  )
}

add_roc_points_from_ratios <- function(data,
                                       fpr,
                                       tpr) {
  add_roc_from_ratios(data, {{ fpr }}, {{ tpr }}, geom_point)
}

#' @title Add a ROC curve plot to an existing one
#' @description
#' Add a ROC curve to an existing ROC plot.
#' @inheritParams plot_roc_curve
#' @examples
#' plot_roc_curve(iris, response = Species, predictor = Sepal.Width) +
#'  add_roc_curve(iris, response = Species, predictor = Sepal.Length)
#' @export
add_roc_curve <- function(data = NULL,
                          fpr = NULL,
                          tpr = NULL,
                          response = NULL,
                          predictor = NULL,
                          .condition = NULL) {
  predictor_expr <- enquo(predictor)
  response_expr <- enquo(response)
  if (!quo_is_null(predictor_expr) && !quo_is_null(response_expr)) {
    data %>%
      add_roc_curve_from_predictor({{ response }}, {{ predictor }}, .condition)
  } else {
    add_roc_curve_from_ratios(data, {{ fpr }}, {{ tpr }})
  }
}

#' @title Add ROC points plot to an existing one
#' @description
#' Add ROC points to an existing ROC plot.
#' @inheritParams plot_roc_points
#' @examples
#' plot_roc_curve(iris, response = Species, predictor = Sepal.Width) +
#'  add_roc_points(iris, response = Species, predictor = Sepal.Length)
#' @export
add_roc_points <- function(data = NULL,
                           fpr = NULL,
                           tpr = NULL,
                           response = NULL,
                           predictor = NULL,
                           .condition = NULL) {
  predictor_expr <- enquo(predictor)
  response_expr <- enquo(response)
  if (!quo_is_null(predictor_expr) && !quo_is_null(response_expr)) {
    data %>%
      add_roc_points_from_predictor({{ response }}, {{ predictor }}, .condition)
  } else {
    add_roc_points_from_ratios(data, {{ fpr }}, {{ tpr }})
  }
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

add_partial_roc_from_ratios_fpr <- function(data,
                                            fpr,
                                            tpr,
                                            threshold,
                                            geom) {
  geom(
    data = data %>% filter({{ fpr }} <= threshold),
    mapping = aes(
      x = {{ fpr }},
      y = {{ tpr }}
    ),
    size = 0.8
  )
}

add_partial_roc_from_ratios_tpr <- function(data,
                                            fpr,
                                            tpr,
                                            threshold,
                                            geom) {
  geom(
    data = data %>% filter({{ tpr }} >= threshold),
    mapping = aes(
      x = {{ fpr }},
      y = {{ tpr }}
    ),
    size = 0.8
  )
}

add_partial_roc_from_ratios <- function(data,
                                        fpr,
                                        tpr,
                                        ratio,
                                        threshold,
                                        geom) {
  if (ratio == "tpr") {
    add_partial_roc_from_ratios_tpr(
      data,
      {{ fpr }},
      {{ tpr }},
      threshold,
      geom
    )
  } else if (ratio == "fpr") {
    add_partial_roc_from_ratios_fpr(
      data,
      {{ fpr }},
      {{ tpr }},
      threshold,
      geom
    )
  }
}

add_partial_roc_from_predictor_tpr <- function(data,
                                               response,
                                               predictor,
                                               threshold,
                                               geom,
                                               .condition = NULL) {
  predictor_name <- as_name(enquo(predictor))
  if (is.null(data)) {
    geom(
      data = . %>%
        roc_points({{ response }}, {{ predictor }}, .condition) %>%
        filter(.data[["tpr"]] >= threshold),
      mapping = aes(
        x = .data[["fpr"]],
        y = .data[["tpr"]],
        color = predictor_name
      ),
      size = 0.8
    )
  } else {
    geom(
      data = data %>%
        roc_points({{ response }}, {{ predictor }}, .condition) %>%
        filter(.data[["tpr"]] >= threshold),
      mapping = aes(
        x = .data[["fpr"]],
        y = .data[["tpr"]],
        color = predictor_name
      ),
      size = 0.8
    )
  }
}

add_partial_roc_from_predictor_fpr <- function(data,
                                               response,
                                               predictor,
                                               threshold,
                                               geom,
                                               .condition = NULL) {
  predictor_name <- as_name(enquo(predictor))
  if (is.null(data)) {
    geom(
      data = . %>%
        roc_points({{ response }}, {{ predictor }}, .condition) %>%
        filter(.data[["fpr"]] <= threshold),
      mapping = aes(
        x = .data[["fpr"]],
        y = .data[["tpr"]],
        color = predictor_name
      ),
      size = 0.8
    )
  } else {
    geom(
      data = roc_points(data, {{ response }}, {{ predictor }}) %>%
        filter(.data[["fpr"]] <= threshold),
      mapping = aes(
        x = .data[["fpr"]],
        y = .data[["tpr"]],
        color = predictor_name
      ),
      size = 0.8
    )
  }
}

add_partial_roc_from_predictor <- function(data,
                                           response,
                                           predictor,
                                           ratio,
                                           threshold,
                                           geom,
                                           .condition = NULL) {
  if (ratio == "tpr") {
    add_partial_roc_from_predictor_tpr(
      data,
      {{ response }},
      {{ predictor }},
      threshold,
      geom,
      .condition
    )
  } else if (ratio == "fpr") {
    add_partial_roc_from_predictor_fpr(
      data,
      {{ response }},
      {{ predictor }},
      threshold,
      geom
    )
  }
}

add_partial_roc <- function(data,
                            fpr,
                            tpr,
                            response,
                            predictor,
                            ratio,
                            threshold,
                            geom,
                            .condition = NULL) {
  predictor_expr <- enquo(predictor)
  response_expr <- enquo(response)
  if (!quo_is_null(predictor_expr) && !quo_is_null(response_expr)) {
    add_partial_roc_from_predictor(
      data,
      {{ response }},
      {{ predictor }},
      ratio,
      threshold,
      geom,
      .condition
    )
  } else {
    add_partial_roc_from_ratios(
      data,
      {{ fpr }},
      {{ tpr }},
      ratio,
      threshold,
      geom
    )
  }
}

#' @title Add a section of a ROC curve to an existing one
#' @description
#' Add an specific region of a ROC curve to an existing ROC plot.
#' @inheritParams plot_partial_roc_curve
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
add_partial_roc_curve <- function(data = NULL,
                                  fpr = NULL,
                                  tpr = NULL,
                                  response = NULL,
                                  predictor = NULL,
                                  ratio,
                                  threshold,
                                  .condition = NULL) {
  add_partial_roc(
    data,
    {{ fpr }},
    {{ tpr }},
    {{ response }},
    {{ predictor }},
    ratio,
    threshold,
    geom_path,
    .condition = NULL
  )
}

#' @title Add points in a section of a ROC curve to an existing plot
#' @description
#' Add points in a specific ROC region to an existing ROC plot.
#' @inheritParams plot_partial_roc_points
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
add_partial_roc_points <- function(data = NULL,
                                   fpr = NULL,
                                   tpr = NULL,
                                   response = NULL,
                                   predictor = NULL,
                                   ratio,
                                   threshold,
                                   .condition = NULL) {
  add_partial_roc(
    data,
    {{ fpr }},
    {{ tpr }},
    {{ response }},
    {{ predictor }},
    ratio,
    threshold,
    geom_point,
    .condition
  )
}

#' @title Plot a section of a classifier ROC curve
#' @description
#' Create a curve plot using points in an specific region of ROC curve.
#' @inheritParams plot_partial_roc_points
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
                                   fpr = NULL,
                                   tpr = NULL,
                                   response = NULL,
                                   predictor = NULL,
                                   ratio,
                                   threshold,
                                   .condition = NULL) {
  plot_roc(data) +
    add_partial_roc_curve(
      data,
      {{ fpr }},
      {{ tpr }},
      {{ response }},
      {{ predictor }},
      ratio,
      threshold,
      .condition
    )
}

#' @title Plot points in a region of a ROC curve.
#' @description
#' Create an scatter plot using points in an specific region of ROC curve.
#' @inheritParams calc_partial_roc_points
#' @inheritParams summarize_predictor
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
                                    fpr = NULL,
                                    tpr = NULL,
                                    response = NULL,
                                    predictor = NULL,
                                    ratio,
                                    threshold,
                                    .condition = NULL) {
  plot_roc(data) +
    add_partial_roc_points(
      data,
      {{ fpr }},
      {{ tpr }},
      {{ response }},
      {{ predictor }},
      ratio,
      threshold,
      .condition
    )
}

#' @rdname fpauc_lower_bounds
#' @export
add_fpauc_partially_proper_lower_bound <- function(data = NULL,
                                                   fpr = NULL,
                                                   tpr = NULL,
                                                   response = NULL,
                                                   predictor = NULL,
                                                   threshold,
                                                   .condition = NULL) {
  predictor_expr <- enquo(predictor)
  response_expr <- enquo(response)

  if (!quo_is_null(predictor_expr) && !quo_is_null(response_expr)) {
    geom_polygon(
      data = tibble(
        x = c(threshold, 1, 1),
        y = c(threshold, 1, threshold)
      ),
      mapping = aes(
        .data$x,
        .data$y,
        color = mask_name(predictor_expr),
        fill = str_c(
          mask_name(predictor_expr),
          " FpAUC lower bound"
        )
      ),
      alpha = 1 / 5,
      linetype = "solid"
    )
  } else {
    geom_polygon(
      data = tibble(
        x = c(threshold, 1, 1),
        y = c(threshold, 1, threshold)
      ),
      mapping = aes(
        .data$x,
        .data$y
      ),
      color = "black",
      alpha = 1 / 5,
      linetype = "solid"
    )
  }
}

#' @rdname fpauc_lower_bounds
#' @export
add_fpauc_concave_lower_bound <- function(data = NULL,
                                          fpr = NULL,
                                          tpr = NULL,
                                          response = NULL,
                                          predictor = NULL,
                                          threshold,
                                          .condition = NULL) {
  predictor_expr <- enquo(predictor)
  response_expr <- enquo(response)

  partial_points <- data %>%
    calc_partial_roc_points(
      {{ fpr }},
      {{ tpr }},
      {{ response }},
      {{ predictor }},
      threshold,
      1,
      "tpr",
      .condition
    )
  threshold_fpr <- partial_points[["partial_fpr"]][1]

  if (!quo_is_null(predictor_expr) && !quo_is_null(response_expr)) {
    geom_polygon(
      data = tibble(
        x = c(threshold_fpr, 1, 1),
        y = c(threshold, threshold, 1)
      ),
      mapping = aes(
        .data$x,
        .data$y,
        color = mask_name(predictor_expr),
        fill = str_c(
          mask_name(predictor_expr),
          " FpAUC lower bound"
        )
      ),
      alpha = 1 / 5,
      linetype = "solid"
    )
  } else {
    geom_polygon(
      data = tibble(
        x = c(threshold_fpr, 1, 1),
        y = c(threshold, threshold, 1)
      ),
      mapping = aes(
        .data$x,
        .data$y
      ),
      color = "black",
      alpha = 1 / 5,
      linetype = "solid"
    )
  }
}



#' @title Add fpauc lower bound to a ROC plot
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
add_fpauc_lower_bound <- function(data = NULL,
                                  fpr = NULL,
                                  tpr = NULL,
                                  response = NULL,
                                  predictor = NULL,
                                  threshold,
                                  .condition = NULL) {
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
      fpr,
      tpr,
      response = {{ response }},
      predictor = {{ predictor }},
      threshold,
      .condition
    )
  } else if (curve_shape == "Partially proper") {
    bound <- add_fpauc_partially_proper_lower_bound(
      data,
      fpr,
      tpr,
      response = {{ response }},
      predictor = {{ predictor }},
      threshold,
      .condition
    )
  } else if (curve_shape == "Hook under chance") {
    bound <- NULL
  }
  bound
}

#' @rdname tpauc_lower_bounds
#' @export
add_tpauc_concave_lower_bound <- function(data = NULL,
                                          fpr = NULL,
                                          tpr = NULL,
                                          response = NULL,
                                          predictor = NULL,
                                          lower_threshold,
                                          upper_threshold,
                                          .condition = NULL) {
  partial_points <- data %>%
    calc_partial_roc_points(
      {{ response }},
      {{ predictor }},
      {{ fpr }},
      {{ tpr }},
      lower_threshold,
      upper_threshold,
      "fpr",
      .condition
    )
  lower_threshold_tpr <- partial_points %>%
    filter(.data$partial_fpr == lower_threshold) %>%
    slice_min(.data$partial_tpr, n = 1, with_ties = FALSE) %>%
    pull(.data$partial_tpr)
  upper_threshold_tpr <- partial_points %>%
    filter(.data$partial_fpr == upper_threshold) %>%
    slice_max(.data$partial_tpr, n = 1, with_ties = FALSE) %>%
    pull(.data$partial_tpr)

  predictor_expr <- enquo(predictor)
  response_expr <- enquo(response)

  if (!quo_is_null(predictor_expr) && !quo_is_null(response_expr)) {
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
        color = mask_name(predictor_expr),
        fill = str_c(
          mask_name(predictor_expr),
          " TpAUC lower bound"
        )
      ),
      alpha = 1 / 5,
      linetype = "solid"
    )
  } else {
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
        .data$y
      ),
      color = "black",
      alpha = 1 / 5,
      linetype = "solid"
    )
  }
}

#' @rdname tpauc_lower_bounds
#' @export
add_tpauc_partially_proper_lower_bound <- function(data = NULL,
                                                   fpr = NULL,
                                                   tpr = NULL,
                                                   response = NULL,
                                                   predictor = NULL,
                                                   lower_threshold,
                                                   upper_threshold,
                                                   .condition = NULL) {
  predictor_expr <- enquo(predictor)
  response_expr <- enquo(response)

  partial_points <- data %>% calc_partial_roc_points(
    {{ response }},
    {{ predictor }},
    {{ fpr }},
    {{ tpr }},
    lower_threshold,
    upper_threshold,
    "fpr",
    .condition
  )
  lower_threshold_tpr <- partial_points %>%
    filter(.data$partial_fpr == lower_threshold) %>%
    slice_min(.data$partial_tpr, n = 1, with_ties = FALSE) %>%
    pull(.data$partial_tpr)

  diagonal_area <- calc_fpr_diagonal_lower_bound(
    partial_points[["partial_fpr"]],
    partial_points[["partial_tpr"]]
  )
  square_area <- calc_fpr_square_lower_bound(
    partial_points[["partial_fpr"]],
    partial_points[["partial_tpr"]]
  )

  if (!quo_is_null(predictor_expr) && !quo_is_null(response_expr)) {
    if (diagonal_area > square_area) {
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
          color = mask_name(predictor_expr),
          fill = str_c(
            mask_name(predictor_expr),
            " TpAUC lower bound"
          )
        ),
        alpha = 1 / 5,
        linetype = "solid"
      )
    } else if (diagonal_area < square_area) {
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
          color = mask_name(predictor_expr),
          fill = str_c(
            mask_name(predictor_expr),
            " TpAUC lower bound"
          )
        ),
        alpha = 1 / 5,
        linetype = "solid"
      )
    }
  } else {
    if (diagonal_area > square_area) {
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
          .data$y
        ),
        color = "black",
        alpha = 1 / 5,
        linetype = "solid"
      )
    } else if (diagonal_area < square_area) {
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
          .data$y
        ),
        color = "black",
        alpha = 1 / 5,
        linetype = "solid"
      )
    }
  }
}

#' @rdname tpauc_lower_bounds
#' @export
add_tpauc_under_chance_lower_bound <- function(data = NULL,
                                               fpr = NULL,
                                               tpr = NULL,
                                               response = NULL,
                                               predictor = NULL,
                                               lower_threshold,
                                               upper_threshold,
                                               .condition = NULL) {
  predictor_expr <- enquo(predictor)
  response_expr <- enquo(response)

  partial_points <- data %>% calc_partial_roc_points(
    {{ response }},
    {{ predictor }},
    {{ fpr }},
    {{ tpr }},
    lower_threshold,
    upper_threshold,
    "fpr",
    .condition
  )
  lower_threshold_tpr <- partial_points %>%
    filter(.data$partial_fpr == lower_threshold) %>%
    slice_min(.data$partial_tpr, n = 1, with_ties = FALSE) %>%
    pull(.data$partial_tpr)

  if (!quo_is_null(response_expr) && !quo_is_null(predictor_expr)) {
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
        color = mask_name(predictor_expr),
        fill = str_c(
          mask_name(predictor_expr),
          " TpAUC lower bound"
        )
      ),
      alpha = 1 / 5,
      linetype = "solid"
    )
  } else {
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
        .data$y
      ),
      color = "black",
      alpha = 1 / 5,
      linetype = "solid"
    )
  }
}

#' @title Add tpauc lower bound to a ROC plot
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
add_tpauc_lower_bound <- function(data = NULL,
                                  fpr = NULL,
                                  tpr = NULL,
                                  response = NULL,
                                  predictor = NULL,
                                  lower_threshold,
                                  upper_threshold,
                                  .condition = NULL) {
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
      data,
      fpr,
      tpr,
      response = {{ response }},
      predictor = {{ predictor }},
      upper_threshold = upper_threshold,
      lower_threshold = lower_threshold,
      .condition = .condition
    )
  } else if (curve_shape == "Partially Proper") {
    bound <- add_tpauc_partially_proper_lower_bound(
      data,
      fpr,
      tpr,
      response = {{ response }},
      predictor = {{ predictor }},
      upper_threshold = upper_threshold,
      lower_threshold = lower_threshold,
      .condition = .condition
    )
  } else if (curve_shape == "Hook under chance") {
    bound <- add_tpauc_under_chance_lower_bound(
      data,
      fpr,
      tpr,
      response = {{ response }},
      predictor = {{ predictor }},
      upper_threshold = upper_threshold,
      lower_threshold = lower_threshold,
      .condition = .condition
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
add_npauc_lower_bound <- function(data = NULL,
                                  fpr = NULL,
                                  tpr = NULL,
                                  response = NULL,
                                  predictor = NULL,
                                  threshold,
                                  .condition = NULL) {
  predictor_expr <- enquo(predictor)
  response_expr <- enquo(response)
  if (!quo_is_null(predictor_expr) && !quo_is_null(response_expr)) {
    geom_polygon(
      data = tibble(
        x = c(threshold, 1, 1),
        y = c(threshold, 1, threshold)
      ),
      mapping = aes(
        .data$x,
        .data$y,
        color = mask_name(predictor_expr),
        fill = str_c(
          mask_name(predictor_expr),
          " NpAUC lower bound"
        )
      ),
      alpha = 1 / 5,
      linetype = "solid"
    )
  } else {
    geom_polygon(
      data = tibble(
        x = c(threshold, 1, 1),
        y = c(threshold, 1, threshold)
      ),
      mapping = aes(
        .data$x,
        .data$y
      ),
      color = "black",
      alpha = 1 / 5,
      linetype = "solid"
    )
  }
}

#' @rdname npauc_lower_bounds
#' @export
add_npauc_normalized_lower_bound <- function(data = NULL,
                                             fpr = NULL,
                                             tpr = NULL,
                                             response = NULL,
                                             predictor = NULL,
                                             threshold,
                                             .condition = NULL) {
  predictor_expr <- enquo(predictor)
  response_expr <- enquo(response)

  if (!quo_is_null(predictor_expr) && !quo_is_null(response_expr)) {
    geom_polygon(
      data = tibble(
        x = c(0, 1, 1),
        y = c(threshold, 1, threshold)
      ),
      mapping = aes(
        .data$x,
        .data$y,
        color = mask_name(predictor_expr),
        fill = str_c(
          mask_name(predictor_expr),
          " NpAUC lower bound"
        )
      ),
      alpha = 1 / 5,
      linetype = "solid"
    )
  } else {
    geom_polygon(
      data = tibble(
        x = c(0, 1, 1),
        y = c(threshold, 1, threshold)
      ),
      mapping = aes(
        .data$x,
        .data$y
      ),
      color = "black",
      alpha = 1 / 5,
      linetype = "solid"
    )
  }
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
#' @inheritParams add_tpauc_lowe_bound
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
add_spauc_lower_bound <- function(data = NULL,
                                  fpr = NULL,
                                  tpr = NULL,
                                  response = NULL,
                                  predictor = NULL,
                                  lower_threshold,
                                  upper_threshold,
                                  .condition = NULL) {
  predictor_expr <- enquo(predictor)
  response_expr <- enquo(response)

  if (!quo_is_null(response_expr) && !quo_is_null(predictor_expr)) {
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
        color = mask_name(predictor_expr),
        fill = str_c(
          mask_name(predictor_expr),
          " SpAUC lower bound"
        )
      ),
      alpha = 1 / 5,
      linetype = "solid"
    )
  } else {
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
        .data$y
      ),
      color = "black",
      alpha = 1 / 5,
      linetype = "solid"
    )
  }
}

#' @title Hide legend in a ROC plot
#' @description Hide legend showing name of ploted classifiers and bounds in a
#' ROC curve plot.
#' @export
hide_legend <- function() {
  theme(legend.position = "none")
}
