GeomRoi <- ggplot2::ggproto(
  "GeomRoi",
  ggplot2::Geom,

  required_aes = character(),

  default_aes = aes(
    fill = "grey70",
    colour = NA,
    alpha = 0.5,
    linewidth = 0.5,
    linetype = 1
  ),

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

      # Only internal boundaries
      vline_data <- data.frame(
        xintercept = fpr[fpr > 0 & fpr < 1]
      )

      hline_data <- data.frame(
        yintercept = tpr[tpr > 0 & tpr < 1]
      )

      # Explicit aesthetics for boundary lines
      boundary_aes <- list(
        colour = "black",
        linewidth = 0.5,
        linetype = "dashed",
        alpha = 1
      )

      if (nrow(vline_data) > 0) {

        vline_data <- cbind(
          vline_data,
          as.data.frame(
            lapply(boundary_aes, rep, nrow(vline_data))
          )
        )

        vlines <- ggplot2::GeomVline$draw_panel(
          vline_data,
          panel_params,
          coord
        )

      } else {
        vlines <- grid::nullGrob()
      }

      if (nrow(hline_data) > 0) {

        hline_data <- cbind(
          hline_data,
          as.data.frame(
            lapply(boundary_aes, rep, nrow(hline_data))
          )
        )

        hlines <- ggplot2::GeomHline$draw_panel(
          hline_data,
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

