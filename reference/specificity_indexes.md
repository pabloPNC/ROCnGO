# Specificity indexes

Specificity indexes provide different ways of calculating area under ROC
curve in a specific FPR region. Two different approaches to calculate
this area are available:

- `tp_auc()` applies *tighter partial area under curve* index (SpAUC).
  This one calculates area under curve adjusting to points defined by
  the curve in the selected region.

- `sp_auc()` applies *standardized partial area under curve* index
  (TpAUC), which calculates area under curve over the whole specified
  region.

## Usage

``` r
sp_auc(
  data = NULL,
  response,
  predictor,
  lower_fpr,
  upper_fpr,
  .condition = NULL,
  .invalid = FALSE
)

tp_auc(
  data = NULL,
  response,
  predictor,
  lower_fpr,
  upper_fpr,
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

- lower_fpr, upper_fpr:

  Two numbers between 0 and 1, inclusive. These numbers represent lower
  and upper values of FPR region where to calculate partial area under
  curve.

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

- .invalid:

  If `FALSE`, the default, `sp_auc()` will return `NA` when ROC curve
  does not fit theoretical bounds and index cannot be applied. If
  `TRUE`, function will force the calculation and return a value despite
  probably being incorrect.

## Value

A numeric value representing the index score for the partial area under
ROC curve.

## References

McClish D. K. Analyzing a Portion of the ROC Curve. *Medical Decision
Making* 9, 190-195 (1989).

Vivo J.-M., Franco M. y Vicari D. Rethinking an ROC partial area index
for evaluating the classification performance at a high specificity
range. *Advances in Data Analysis and Classification* 12, 683-704
(2018).

## Examples

``` r
# Calculate sp_auc of Sepal.Width as a classifier of setosa species
# in FPR = (0.9, 1)
sp_auc(
 iris,
 response = Species,
 predictor = Sepal.Width,
 lower_fpr = 0,
 upper_fpr = 0.1
)
#> ℹ Lower 0 and upper 0.1 thresholds already included in points
#> • Skipping lower and upper threshold interpolation
#> [1] 0.7242105
# Calculate tp_auc of Sepal.Width as a classifier of setosa species
 # in FPR = (0.9, 1)
tp_auc(
 iris,
 response = Species,
 predictor = Sepal.Width,
 lower_fpr = 0,
 upper_fpr = 0.1
)
#> ℹ Lower 0 and upper 0.1 thresholds already included in points
#> • Skipping lower and upper threshold interpolation
#> [1] 0.7212121
```
