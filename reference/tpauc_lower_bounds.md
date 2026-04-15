# Add TpAUC lower bound to a ROC plot

Calculate and plot lower bound defined by TpAUC specificity index.

- `add_tpauc_lower_bound()` provides a upper level function which
  automatically calculates curve shape and plots a lower bound that
  better fits it.

Additionally, several lower level functions are provided to plot
specific lower bounds:

- `add_tpauc_concave_lower_bound()`. Plot lower bound corresponding to a
  ROC curve with concave shape in selected region.

- `add_tpauc_partially_proper_lower_bound`. Plot lower bound
  corresponding to a ROC curve with partially proper (presence of some
  hook) in selected region.

- `add_tpauc_under_chance_lower_bound`. Plot lower bound corresponding
  to a ROC curve with a hook under chance line in selected region.

## Usage

``` r
add_tpauc_concave_lower_bound(
  data,
  response = NULL,
  predictor = NULL,
  lower_threshold,
  upper_threshold,
  .condition = NULL,
  .label = NULL
)

add_tpauc_partially_proper_lower_bound(
  data,
  response = NULL,
  predictor = NULL,
  lower_threshold,
  upper_threshold,
  .condition = NULL,
  .label = NULL
)

add_tpauc_under_chance_lower_bound(
  data,
  response = NULL,
  predictor = NULL,
  lower_threshold,
  upper_threshold,
  .condition = NULL,
  .label = NULL
)

add_tpauc_lower_bound(
  data,
  response = NULL,
  predictor = NULL,
  lower_threshold,
  upper_threshold,
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

- lower_threshold, upper_threshold:

  Two numbers between 0 and 1, inclusive. These numbers represent lower
  and upper values of FPR region where to calculate and plot lower
  bound.

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
  add_tpauc_lower_bound(
    data = iris,
    response = Species,
    predictor = Sepal.Width,
    upper_threshold =  0.1,
    lower_threshold = 0
  )
#> ℹ Lower 0 and upper 0.1 thresholds already included in points
#> • Skipping lower and upper threshold interpolation
#> ℹ Lower 0 and upper 0.1 thresholds already included in points
#> • Skipping lower and upper threshold interpolation
```
