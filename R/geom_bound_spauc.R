GeomBoundSpauc <- ggproto("GeomBoundSpauc", Geom,
  extra_params = c("na.rm", "fpr_threshold", "tpr_threshold"),
  default_aes = aes(
    colour = Predictor,
    fill = "grey50",
    linewidth = 0.5,
    linetype = 1,
    alpha = 0.2
  ),
  setup_data = function(data, params) {
    cli::cli_h1("setup_data")
    print(as_tibble(data))
    fprt <- params$fpr_threshold

    coordinates <- data.frame(
      x = c(fprt[1], fprt[2], fprt[2], fprt[1]),
      y = c(0, 0, fprt[2], fprt[1])
    )

    data_bound <- tidyr::expand_grid(
      coordinates,
      Predictor = unique(data$Predictor),
      PANEL = unique(data$PANEL),
      group = unique(data$group),
      colour = unique(data$colour)
    )

    print(as_tibble(data_bound))
    data_bound
  },

  setup_params = function(data, params) {
    cli::cli_h1("setup_params")
    print(params)
    params
  },
  draw_panel = function(data, panel_params, coord) {
    cli::cli_h1("draw_panel")
    print(as_tibble(data))
    GeomPolygon$draw_panel(data, panel_params, coord)
  }
)