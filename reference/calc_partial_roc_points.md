# Calculate ROC curve partial points

Calculates a series pairs of (FPR, TPR) which correspond to ROC curve
points in a specified region.

## Usage

``` r
calc_partial_roc_points(
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

A tibble with two columns:

- "tpr". Containing "true positive ratio", or y, values of points within
  the specified region.

- "fpr". Containing "false positive ratio", or x, values of points
  within the specified region.

## Examples

``` r
# Calc ROC points of Sepal.Width as a classifier of setosa species
# in TPR = (0.9, 1)
calc_partial_roc_points(
 iris,
 response = Species,
 predictor = Sepal.Width,
 lower_threshold = 0.9,
 upper_threshold = 1,
 ratio = "tpr"
)
#> ℹ Upper threshold 1 already included in points.
#> • Skipping upper threshold interpolation
#> # A tibble: 59 × 2
#>      tpr   fpr
#>  * <dbl> <dbl>
#>  1  0.9   0.35
#>  2  0.96  0.45
#>  3  0.96  0.45
#>  4  0.96  0.45
#>  5  0.96  0.45
#>  6  0.96  0.45
#>  7  0.96  0.45
#>  8  0.96  0.45
#>  9  0.96  0.45
#> 10  0.96  0.45
#> # ℹ 49 more rows

# Change class to virginica
calc_partial_roc_points(
 iris,
 response = Species,
 predictor = Sepal.Width,
 lower_threshold = 0.9,
 upper_threshold = 1,
 ratio = "tpr",
 .condition = "virginica"
)
#> ℹ Lower 0.9 and upper 1 thresholds already included in points
#> • Skipping lower and upper threshold interpolation
#> # A tibble: 20 × 2
#>      tpr   fpr
#>  * <dbl> <dbl>
#>  1  0.9   0.86
#>  2  0.9   0.86
#>  3  0.9   0.86
#>  4  0.9   0.86
#>  5  0.9   0.86
#>  6  0.9   0.86
#>  7  0.9   0.86
#>  8  0.9   0.86
#>  9  0.98  0.9 
#> 10  0.98  0.9 
#> 11  0.98  0.9 
#> 12  0.98  0.93
#> 13  0.98  0.93
#> 14  0.98  0.93
#> 15  0.98  0.93
#> 16  0.98  0.97
#> 17  0.98  0.97
#> 18  0.98  0.97
#> 19  1     0.99
#> 20  1     1   
```
