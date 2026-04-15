# Summarize classifier performance

Calculates a series of metrics describing global and local classifier
performance.

## Usage

``` r
summarize_predictor(
  data = NULL,
  predictor,
  response,
  ratio,
  threshold,
  .condition = NULL
)
```

## Arguments

- data:

  A data.frame or extension (e.g. a tibble) containing values for
  predictors and response variables.

- predictor:

  A data variable which must be numeric, representing values of a
  classifier or predictor for each observation.

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

## Value

A single row tibble with different predictor with following metrics as
columns:

- Area under curve (AUC) as a metric of global performance.

- Partial are under curve (pAUC) as a metric of local performance.

- Indexes derived from pAUC, depending on the selected ratio.
  [Sensitivity
  indexes](https://pablopnc.github.io/ROCnGO/reference/sensitivity_indexes.md)
  will be used for TPR and [specificity
  indexes](https://pablopnc.github.io/ROCnGO/reference/specificity_indexes.md)
  for FPR.

- [Curve
  shape](https://pablopnc.github.io/ROCnGO/reference/calc_curve_shape.md)
  in the specified region.

## Examples

``` r
# Summarize Sepal.Width as a classifier of setosa species
# and local performance in TPR (0.9, 1)
summarize_predictor(
 data = iris,
 predictor = Sepal.Width,
 response = Species,
 ratio = "tpr",
 threshold = 0.9
)
#> ℹ Upper threshold 1 already included in points.
#> • Skipping upper threshold interpolation
#> # A tibble: 1 × 6
#>     auc   pauc np_auc fp_auc ncp_auc curve_shape     
#>   <dbl>  <dbl>  <dbl>  <dbl>   <dbl> <chr>           
#> 1 0.880 0.0472  0.472  0.852   0.906 Partially proper
# Summarize Sepal.Width as a classifier of setosa species
# and local performance in FPR (0, 0.1)
summarize_predictor(
 data = iris,
 predictor = Sepal.Width,
 response = Species,
 ratio = "fpr",
 threshold = 0.1
)
#> ℹ Lower 0 and upper 0.1 thresholds already included in points
#> • Skipping lower and upper threshold interpolation
#> # A tibble: 1 × 6
#>     auc   pauc sp_auc tp_auc ncp_auc curve_shape
#>   <dbl>  <dbl>  <dbl>  <dbl>   <dbl> <chr>      
#> 1 0.880 0.0476  0.724  0.721   0.907 Concave    
```
