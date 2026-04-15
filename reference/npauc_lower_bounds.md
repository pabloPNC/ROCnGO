# Add NpAUC lower bound to a ROC plot

Calculate and plot lower bound defined by NpAUC specificity index.

- `add_npauc_normalized_lower_bound()` allows to plot normalized lower
  bound, which is used to formally calculate NpAUC.

- `add_npauc_lower_bound()` is a lower level function providing a way to
  plot lower bound previous to normalization.

## Usage

``` r
add_npauc_lower_bound(
  data,
  response = NULL,
  predictor = NULL,
  threshold,
  .condition = NULL,
  .label = NULL
)

add_npauc_normalized_lower_bound(
  data,
  response = NULL,
  predictor = NULL,
  threshold,
  .condition = NULL,
  .label = NULL
)
```

## Arguments

- data:

  A data.frame or extension (e.g. a tibble) containing values for
  predictors and response variables.

- response:

  A data variable which must be a factor, integer or character vector
  representing the prediction outcome on each observation (*Gold
  Standard*).

  If the variable presents more than two possible outcomes, classes or
  categories:

  - The outcome of interest (the one to be predicted) will remain
    distinct.

  - All other categories will be combined into a single category.

  New combined category represents the "absence" of the condition to
  predict. See `.condition` for more information.

- predictor:

  A data variable which must be numeric, representing values of a
  classifier or predictor for each observation.

- threshold:

  A number between 0 and 1, inclusive. This number represents the lower
  value of TPR for the region where to calculate and plot lower bound.

  Because of definition of
  [`np_auc()`](https://pablopnc.github.io/ROCnGO/reference/sensitivity_indexes.md),
  region upper bound will be established as 1.

- .condition:

  A value from response that represents class, category or condition of
  interest which wants to be predicted.

  If `NULL`, condition of interest will be selected automatically
  depending on `response` type.

  Once the class of interest is selected, rest of them will be collapsed
  in a common category, representing the "absence" of the condition to
  be predicted.

  See
  [`vignette("selecting-condition")`](https://pablopnc.github.io/ROCnGO/articles/selecting-condition.md)
  for further information on how automatic selection is performed and
  details on selecting the condition of interest.

- .label:

  A string representing the name used in labels.

  If `NULL`, variable name from `predictor` will be used as label.

## Value

A ggplot layer instance object.

## Examples

``` r
plot_roc_curve(iris, response = Species, predictor = Sepal.Width) +
  add_npauc_lower_bound(
    iris,
    response = Species,
    predictor = Sepal.Width,
    threshold = 0.9
  )
```
