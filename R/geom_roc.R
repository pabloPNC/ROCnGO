recompute_groups <- function(data, exclude_response = TRUE) {

  discrete <- vapply(
    data,
    \(x) is.factor(x) || is.character(x) || is.logical(x),
    logical(1)
  )

  excluded <- c("label", "PANEL", "group")

  if (exclude_response) {
    excluded <- c(excluded, "response")
  }

  discrete[names(discrete) %in% excluded] <- FALSE

  group_vars <- data[discrete]

  if (length(group_vars) == 0L) {
    return(rep(1L, nrow(data)))
  }

  interaction(
    group_vars,
    drop = TRUE,
    lex.order = TRUE
  )
}

StatRoc <- ggproto("StatRoc", Stat,
  required_aes = c("response", "predictor"),
  dropped_aes = c("response", "predictor"),
  default_aes = aes(
    x = after_stat(fpr),
    y = after_stat(tpr)
  ),
  setup_data = function(data, params) {

    automatic_group <- recompute_groups(
      data,
      exclude_response = FALSE
    )

    desired_group <- recompute_groups(
      data,
      exclude_response = TRUE
    )

    if (identical(
      as.integer(interaction(data$group, drop = TRUE)),
      as.integer(interaction(automatic_group, drop = TRUE))
    )) {
      data$group <- as.integer(desired_group)
    }
    print(as_tibble(data))
    data
  },
  compute_group = function(data, scales, .condition = NULL) {
    x <- roc_points(
      data = data,
      response = response,
      predictor = predictor,
      .condition = .condition
    )
    x
  }
)

stat_roc <- function(mapping = NULL,
                     data = NULL,
                     geom = "point",
                     position = "identity",
                     ...,
                     .condition = NULL,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {

  layer(
    stat = StatRoc,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      .condition = .condition,
      na.rm = na.rm,
      ...
    )
  )
}

GeomRoc <- ggproto("GeomRoc", GeomPath,

  required_aes = c("x", "y"),

  default_aes = aes(
    colour = "black",
    fill = NA,
    linewidth = 0.5,
    linetype = 1,
    alpha = NA
  ),
  draw_panel = function(data, panel_params, coord,
                        reference = FALSE,
                        reference_colour = "grey70",
                        reference_linetype = 2,
                        reference_linewidth = 0.5,
                        arrow = NULL,
                        lineend = "butt",
                        linejoin = "round",
                        linemitre = 10,
                        na.rm = FALSE) {
    grobs <- list()

    # Area under the ROC curve
    if ("fill" %in% names(data) && any(!is.na(data$fill))) {

      groups <- split(data, data$group)  # generates a dfs for each group

      polygon_data <- do.call(
        rbind,

        lapply(groups, function(d) {  # closes polygon
          # Close the polygon along the x-axis
          polygon <- rbind(
            d,
            d[nrow(d), , drop = FALSE],
            d[1, , drop = FALSE]
          )  # adds polygon two extra points of FPR (closing it)
          polygon$y[(nrow(d) + 1):nrow(polygon)] <- 0  # new points are TPR 0
          polygon$colour <- NA
          polygon
        })
      )

      grobs$area <- GeomPolygon$draw_panel(
        polygon_data,
        panel_params,
        coord
        # na.rm = na.rm  # not working
      )
    }
    # Random classifier reference line
    if (reference) {
      reference_data <- data.frame(
        x = c(0, 1),
        y = c(0, 1),
        group = 1,
        colour = reference_colour,
        linewidth = reference_linewidth,
        linetype = reference_linetype,
        alpha = 1
      )
      grobs$reference <- GeomPath$draw_panel(
        reference_data,
        panel_params,
        coord,
        na.rm = na.rm
      )
    }
    # ROC curve
    grobs$roc <- GeomPath$draw_panel(
      data,
      panel_params,
      coord,
      arrow = arrow,
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      na.rm = na.rm
    )

    grid::grobTree(
      do.call(grid::gList, grobs)
    )
  }
)

geom_roc <- function(mapping = NULL,
                     data = NULL,
                     stat = "roc",
                     position = "identity",
                     ...,
                     reference = FALSE,
                     reference_colour = "grey70",
                     reference_linetype = 2,
                     reference_linewidth = 0.5,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {

  layer(
    geom = GeomRoc,
    stat = stat,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      reference = reference,
      reference_colour = reference_colour,
      reference_linetype = reference_linetype,
      reference_linewidth = reference_linewidth,
      na.rm = na.rm,
      ...
    )
  )
}
