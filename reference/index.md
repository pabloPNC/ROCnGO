# Package index

## ROC curve

Functions that operate on the entire ROC curve.

- [`auc()`](https://pablopnc.github.io/ROCnGO/reference/auc.md) :
  Calculate area under ROC curve
- [`roc_points()`](https://pablopnc.github.io/ROCnGO/reference/roc_points.md)
  : Calculate ROC curve points

## Partial ROC curve

Functions that operate on an specific ROC curve region.

- [`pauc()`](https://pablopnc.github.io/ROCnGO/reference/pauc.md) :
  Calculate partial area under curve
- [`calc_partial_roc_points()`](https://pablopnc.github.io/ROCnGO/reference/calc_partial_roc_points.md)
  : Calculate ROC curve partial points
- [`calc_curve_shape()`](https://pablopnc.github.io/ROCnGO/reference/calc_curve_shape.md)
  : Calculate curve shape over an specific region

### Indexes

Local performance metrics.

- [`fp_auc()`](https://pablopnc.github.io/ROCnGO/reference/sensitivity_indexes.md)
  [`np_auc()`](https://pablopnc.github.io/ROCnGO/reference/sensitivity_indexes.md)
  : Sensitivity indexes
- [`sp_auc()`](https://pablopnc.github.io/ROCnGO/reference/specificity_indexes.md)
  [`tp_auc()`](https://pablopnc.github.io/ROCnGO/reference/specificity_indexes.md)
  : Specificity indexes
- [`cp_auc()`](https://pablopnc.github.io/ROCnGO/reference/concordance_indexes.md)
  [`ncp_auc()`](https://pablopnc.github.io/ROCnGO/reference/concordance_indexes.md)
  : Concordance indexes

## Summarize

Functions to quickly summarize a classifier.

- [`summarize_predictor()`](https://pablopnc.github.io/ROCnGO/reference/summarize_predictor.md)
  : Summarize classifier performance
- [`summarize_dataset()`](https://pablopnc.github.io/ROCnGO/reference/summarize_dataset.md)
  : Summarize classifiers performance in a dataset

## Plot

Functions to plot ROC curves of classifiers and derived metrics.

### Initialize plot

- [`plot_roc_points()`](https://pablopnc.github.io/ROCnGO/reference/plot_roc_points.md)
  : Plot classifier points of a ROC curve
- [`plot_roc_curve()`](https://pablopnc.github.io/ROCnGO/reference/plot_roc_curve.md)
  : Plot a classifier ROC curve
- [`plot_partial_roc_points()`](https://pablopnc.github.io/ROCnGO/reference/plot_partial_roc_points.md)
  : Plot points in a region of a ROC curve
- [`plot_partial_roc_curve()`](https://pablopnc.github.io/ROCnGO/reference/plot_partial_roc_curve.md)
  : Plot a section of a classifier ROC curve

### Add points to plot

- [`add_roc_points()`](https://pablopnc.github.io/ROCnGO/reference/add_roc_points.md)
  : Add ROC points plot to an existing one
- [`add_roc_curve()`](https://pablopnc.github.io/ROCnGO/reference/add_roc_curve.md)
  : Add a ROC curve plot to an existing one
- [`add_partial_roc_points()`](https://pablopnc.github.io/ROCnGO/reference/add_partial_roc_points.md)
  : Add points in a section of a ROC curve to an existing plot
- [`add_partial_roc_curve()`](https://pablopnc.github.io/ROCnGO/reference/add_partial_roc_curve.md)
  : Add a section of a ROC curve to an existing one

### Add index bounds

- [`add_fpauc_partially_proper_lower_bound()`](https://pablopnc.github.io/ROCnGO/reference/fpauc_lower_bounds.md)
  [`add_fpauc_concave_lower_bound()`](https://pablopnc.github.io/ROCnGO/reference/fpauc_lower_bounds.md)
  [`add_fpauc_lower_bound()`](https://pablopnc.github.io/ROCnGO/reference/fpauc_lower_bounds.md)
  : Add FpAUC lower bound to a ROC plot
- [`add_npauc_lower_bound()`](https://pablopnc.github.io/ROCnGO/reference/npauc_lower_bounds.md)
  [`add_npauc_normalized_lower_bound()`](https://pablopnc.github.io/ROCnGO/reference/npauc_lower_bounds.md)
  : Add NpAUC lower bound to a ROC plot
- [`add_tpauc_concave_lower_bound()`](https://pablopnc.github.io/ROCnGO/reference/tpauc_lower_bounds.md)
  [`add_tpauc_partially_proper_lower_bound()`](https://pablopnc.github.io/ROCnGO/reference/tpauc_lower_bounds.md)
  [`add_tpauc_under_chance_lower_bound()`](https://pablopnc.github.io/ROCnGO/reference/tpauc_lower_bounds.md)
  [`add_tpauc_lower_bound()`](https://pablopnc.github.io/ROCnGO/reference/tpauc_lower_bounds.md)
  : Add TpAUC lower bound to a ROC plot
- [`add_spauc_lower_bound()`](https://pablopnc.github.io/ROCnGO/reference/spauc_lower_bounds.md)
  : Add SpAUC lower bound to a ROC plot

### Customize plot

- [`add_chance_line()`](https://pablopnc.github.io/ROCnGO/reference/add_chance_line.md)
  : Show chance line in a ROC plot
- [`add_fpr_threshold_line()`](https://pablopnc.github.io/ROCnGO/reference/plot_thresholds.md)
  [`add_tpr_threshold_line()`](https://pablopnc.github.io/ROCnGO/reference/plot_thresholds.md)
  [`add_threshold_line()`](https://pablopnc.github.io/ROCnGO/reference/plot_thresholds.md)
  : Add a threshold line to a ROC plot
- [`hide_legend()`](https://pablopnc.github.io/ROCnGO/reference/hide_legend.md)
  : Hide legend in a ROC plot

## Built in datasets

- [`prost`](https://pablopnc.github.io/ROCnGO/reference/prost.md) :
  Prostate cancer gene expression data

## Other helpers

- [`sumexp_to_df()`](https://pablopnc.github.io/ROCnGO/reference/sumexp_to_df.md)
  : Transform data in a SummarizedExperiment to a data.frame
