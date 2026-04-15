# Calculate partial area under curve

Calculates area under curve curve in an specific TPR or FPR region.

## Usage

``` r
pauc(
  data = NULL,
  response,
  predictor,
  ratio,
  lower_threshold,
  upper_threshold,
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

- ratio:

  Ratio or axis where to apply calculations.

  - If `"tpr"`, only points within the specified region of TPR, y axis,
    will be considered for calculations.

  - If `"fpr"`, only points within the specified region of FPR, x axis,
    will be considered for calculations.

- lower_threshold, upper_threshold:

  Two numbers between 0 and 1, inclusive. These numbers represent lower
  and upper bounds of the region where to apply calculations.

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

A numeric value representing the area under ROC curve in the specified
region.

## Examples

``` r
# Calculate pauc of Sepal.Width as a classifier of setosa species in
# in TPR = (0.9, 1)
pauc(
  iris,
  response = Species,
  predictor = Sepal.Width,
  ratio = "tpr",
  lower_threshold = 0.9,
  upper_threshold = 1
)
#> ℹ Upper threshold 1 already included in points.
#> • Skipping upper threshold interpolation
#> [1] 0.0472
# Calculate pauc of Sepal.Width as a classifier of setosa species in
# in FPR = (0, 0.1)
pauc(
  iris,
  response = Species,
  predictor = Sepal.Width,
  ratio = "fpr",
  lower_threshold = 0,
  upper_threshold = 0.1
)
#> ℹ Lower 0 and upper 0.1 thresholds already included in points
#> • Skipping lower and upper threshold interpolation
#> [1] 0.0476
```
