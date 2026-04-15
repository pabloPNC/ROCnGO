# Calculate ROC curve points

Calculates a series pairs of (FPR, TPR) which correspond to points
displayed by ROC curve. "false positive ratio" will be represented on x
axis, while "true positive ratio" on y one.

## Usage

``` r
roc_points(data = NULL, response, predictor, .condition = NULL)
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

- "tpr". Containing values for "true positive ratio", or y axis.

- "fpr". Containing values for "false positive ratio", or x axis.

## Examples

``` r
# Calc ROC points of Sepal.Width as a classifier of setosa species
roc_points(iris, Species, Sepal.Width)
#> # A tibble: 151 × 2
#>      tpr   fpr
#>  * <dbl> <dbl>
#>  1  1     1   
#>  2  1     0.99
#>  3  1     0.96
#>  4  1     0.96
#>  5  1     0.96
#>  6  0.98  0.93
#>  7  0.98  0.93
#>  8  0.98  0.93
#>  9  0.98  0.93
#> 10  0.98  0.9 
#> # ℹ 141 more rows
# Change class to predict to virginica
roc_points(iris, Species, Sepal.Width, .condition = "virginica")
#> # A tibble: 151 × 2
#>      tpr   fpr
#>  * <dbl> <dbl>
#>  1  1     1   
#>  2  1     0.99
#>  3  0.98  0.97
#>  4  0.98  0.97
#>  5  0.98  0.97
#>  6  0.98  0.93
#>  7  0.98  0.93
#>  8  0.98  0.93
#>  9  0.98  0.93
#> 10  0.98  0.9 
#> # ℹ 141 more rows
```
