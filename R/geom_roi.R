GeomRoi <- ggplot2::ggproto(
  "GeomRoi",
  ggplot2::Geom,

  required_aes = character(),

  default_aes = ggplot2::aes(
    fill = "grey70",
    colour = NA,
    alpha = 0.5,
    linewidth = 0.5,
    linetype = 1
  ),

  draw_key = ggplot2::draw_key_polygon,

  draw_panel = function(data, panel_params, coord,
                        fpr, tpr,
                        show_boundary = TRUE,
                        na.rm = FALSE) {

    # Complementary intervals
    x_intervals <- list(
      c(0, fpr[1]),
      c(fpr[2], 1)
    )

    y_intervals <- list(
      c(0, tpr[1]),
      c(tpr[2], 1)
    )

    # Generate shaded rectangles
    rects <- expand.grid(
      x = seq_along(x_intervals),
      y = seq_along(y_intervals)
    )

    polygons <- lapply(seq_len(nrow(rects)), function(i) {

      x <- x_intervals[[rects$x[i]]]
      y <- y_intervals[[rects$y[i]]]

      if (x[1] >= x[2] || y[1] >= y[2]) {
        return(NULL)
      }

      data.frame(
        x = c(x[1], x[2], x[2], x[1]),
        y = c(y[1], y[1], y[2], y[2]),
        group = i
      )
    })

    polygons <- Filter(Negate(is.null), polygons)

    # Draw shaded area
    if (length(polygons) > 0) {

      polygon_data <- do.call(rbind, polygons)

      aesthetics <- data[
        rep(1, nrow(polygon_data)),
        setdiff(names(data), c("x", "y", "group")),
        drop = FALSE
      ]

      polygon_data <- cbind(aesthetics, polygon_data)

      shaded <- ggplot2::GeomPolygon$draw_panel(
        polygon_data,
        panel_params,
        coord
      )

    } else {
      shaded <- grid::nullGrob()
    }

    # Draw ROI boundaries
    if (show_boundary) {

      # Only internal boundaries are drawn
      vline_data <- data.frame(
        xintercept = fpr[fpr > 0 & fpr < 1]
      )

      hline_data <- data.frame(
        yintercept = tpr[tpr > 0 & tpr < 1]
      )

      # Dedicated aesthetics for boundaries
      boundary_aes <- data.frame(
        colour = "black",
        linewidth = 0.5,
        linetype = "dashed"
      )

      if (nrow(vline_data) > 0) {

        vlines <- ggplot2::GeomVline$draw_panel(
          cbind(vline_data, boundary_aes[rep(1, nrow(vline_data)), ]),
          panel_params,
          coord
        )

      } else {
        vlines <- grid::nullGrob()
      }

      if (nrow(hline_data) > 0) {

        hlines <- ggplot2::GeomHline$draw_panel(
          cbind(hline_data, boundary_aes[rep(1, nrow(hline_data)), ]),
          panel_params,
          coord
        )

      } else {
        hlines <- grid::nullGrob()
      }

    } else {
      vlines <- grid::nullGrob()
      hlines <- grid::nullGrob()
    }

    grid::grobTree(
      shaded,
      vlines,
      hlines
    )
  }
)



geom_roi <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  fpr = c(0, 1),
  tpr = c(0, 1),
  show_boundary = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE
) {

  validate_range <- function(x, name) {

    if (
      !is.numeric(x) ||
      length(x) != 2 ||
      any(!is.finite(x)) ||
      x[1] < 0 ||
      x[2] > 1 ||
      x[1] >= x[2]
    ) {
      stop(
        "`", name,
        "` must be a numeric vector of length 2, ",
        "with increasing values between 0 and 1.",
        call. = FALSE
      )
    }
  }

  validate_range(fpr, "fpr")
  validate_range(tpr, "tpr")

  if (!is.logical(show_boundary) ||
      length(show_boundary) != 1 ||
      is.na(show_boundary)) {
    stop("`show_boundary` must be TRUE or FALSE.",
         call. = FALSE)
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRoi,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fpr = fpr,
      tpr = tpr,
      show_boundary = show_boundary,
      na.rm = na.rm,
      ...
    )
  )
}

