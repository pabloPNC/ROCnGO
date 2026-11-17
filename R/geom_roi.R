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
                        fpr, tpr, na.rm = FALSE) {

    # calculate complementary intervals
    x_intervals <- list(
      c(0, fpr[1]),
      c(fpr[2], 1)
    )

    y_intervals <- list(
      c(0, tpr[1]),
      c(tpr[2], 1)
    )

    # Generate the shaded rectangles
    rects <- expand.grid(
      x = seq_along(x_intervals),
      y = seq_along(y_intervals)
    )

    polygons <- lapply(seq_len(nrow(rects)), function(i) {

      x <- x_intervals[[rects$x[i]]]
      y <- y_intervals[[rects$y[i]]]

      # Skip empty intervals
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

    if (length(polygons) == 0) {
      return(grid::nullGrob())
    }

    polygon_data <- do.call(rbind, polygons)

    # Preserve layer aesthetics
    aesthetics <- data[
      rep(1, nrow(polygon_data)),
      setdiff(names(data), c("x", "y", "group")),
      drop = FALSE
    ]

    polygon_data <- cbind(
      aesthetics,
      polygon_data
    )

    # Draw polygons
    ggplot2::GeomPolygon$draw_panel(
      polygon_data,
      panel_params,
      coord
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
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE
) {

  # Validate ROI intervals
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

  layer(
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
      na.rm = na.rm,
      ...
    )
  )
}
