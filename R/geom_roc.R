GeomRoc <- ggproto("GeomRoc", GeomPath,

  required_aes = c("fpr", "tpr"),

  default_aes = aes(
    color = "black",
    fill = NA,
    linewidth = 0.5,
    linetype = 1,
    alpha = NA
  ),
  setup_data = function(data, params) {
    cli::cli_h1("GeomRoc setup_data")
    print(as_tibble(data))
    data
  },
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

    grobTree(
      do.call(grid::gList, grobs)
    )
  }
)

geom_roc <- function(mapping = NULL,
                     data = NULL,
                     stat = "roc",
                     position = "identity",
                     ...,
                     predictor_name = NULL,
                     reference = FALSE,
                     reference_colour = "grey70",
                     reference_linetype = 2,
                     reference_linewidth = 0.5,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {

  if (is.null(predictor_name) &&
        !is.null(mapping$predictor)) {
    predictor_name <- as_label(mapping$predictor)
  }

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
      predictor_name = predictor_name,
      ...
    )
  )
}
