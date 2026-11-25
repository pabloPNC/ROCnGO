GeomBoundTpauc <- ggproto("GeomBoundTpauc", Geom,
  extra_params = c("na.rm", "fpr_threshold", "tpr_threshold"),
  default_aes = aes(
    colour = Predictor,
    fill = "grey50",
    linewidth = 0.5,
    linetype = 1,
    alpha = 0.2
  ),
  setup_data = function(data, params) {
    cli::cli_h1("GeomBoundTpauc setup_data")
    print(as_tibble(data))
  }
)