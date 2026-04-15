# Calculate curve shape over an specific region

`calc_curve_shape()` calculates ROC curve shape over a specified region.

## Usage

``` r
calc_curve_shape(
  data = NULL,
  response = NULL,
  predictor = NULL,
  lower_threshold,
  upper_threshold,
  ratio,
  .condition = NULL
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
  and upper bounds of the region where to apply calculations.

- ratio:

  Ratio or axis where to apply calculations.

  - If `"tpr"`, only points within the specified region of TPR, y axis,
    will be considered for calculations.

  - If `"fpr"`, only points within the specified region of FPR, x axis,
    will be considered for calculations.

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

## Value

A string indicating ROC curve shape in the specified region. Result can
take any of the following values:

- `"Concave"`. ROC curve is concave over the entire specified region.

- `"Partially proper"`. ROC curve loses concavity at some point of the
  specified region.

- `"Hook under chance"`. ROC curve loses concavity at some point of the
  region and it lies below chance line.

## Examples

``` r
# Calc ROC curve shape of Sepal.Width as a classifier of setosa species
# in TPR = (0.9, 1)
calc_curve_shape(iris, Species, Sepal.Width, 0.9, 1, "tpr")
#> ℹ Upper threshold 1 already included in points.
#> • Skipping upper threshold interpolation
#> [1] "Partially proper"
# Change class to virginica
calc_curve_shape(iris, Species, Sepal.Width, 0.9, 1, "tpr", .condition = "virginica")
#> ℹ Lower 0.9 and upper 1 thresholds already included in points
#> • Skipping lower and upper threshold interpolation
#> [1] "Concave"
```
