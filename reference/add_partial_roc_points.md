# Add points in a section of a ROC curve to an existing plot

Add points in a specific ROC region to an existing ROC plot.

## Usage

``` r
add_partial_roc_points(
  data,
  response = NULL,
  predictor = NULL,
  ratio,
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

- ratio:

  Ratio or axis where to apply calculations.

  - If `"tpr"`, only points within the specified region of TPR, y axis,
    will be considered for calculations.

  - If `"fpr"`, only points within the specified region of FPR, x axis,
    will be considered for calculations.

- threshold:

  A number between 0 and 1, both inclusive, which represents the region
  bound where to calculate partial area under curve.

  If `ratio = "tpr"`, it represents lower bound of the TPR region, being
  its upper limit equal to 1.

  If `ratio = "fpr"`, it represents the upper bound of the FPR region,
  being its lower limit equal to 0.

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
  add_partial_roc_points(
    iris,
    response = Species,
    predictor = Sepal.Length,
    ratio = "tpr",
    threshold = 0.9
  )
#> Ignoring unknown labels:
#> • fill : "Bound"
```
