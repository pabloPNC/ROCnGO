# Sensitivity indexes

Sensitivity indexes provide different ways of calculating area under ROC
curve in a specific TPR region. Two different approaches to calculate
this area are available:

- `fp_auc()` applies *fitted partial area under curve* index (FpAUC).
  This one calculates area under curve adjusting to points defined by
  the curve in the selected region.

- `np_auc()` applies *normalized partial area under curve* index
  (NpAUC), which calculates area under curve over the whole specified
  region.

## Usage

``` r
fp_auc(data = NULL, response, predictor, lower_tpr, .condition = NULL)

np_auc(data, response, predictor, lower_tpr, .condition = NULL)
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

- lower_tpr:

  A numeric value between 0 and 1, inclusive, which represents lower
  value of TPR for the region where to calculate the partial area under
  curve.

  Because of definition of sensitivity indexes, upper bound of the
  region will be established as 1.

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

A numeric value representing the index score for the partial area under
ROC curve.

## References

Franco M. y Vivo J.-M. Evaluating the Performances of Biomarkers over a
Restricted Domain of High Sensitivity. *Mathematics* 9, 2826 (2021).

Jiang Y., Metz C. E. y Nishikawa R. M. A receiver operating
characteristic partial area index for highly sensitive diagnostic tests.
*Radiology* 201, 745-750 (1996).

## Examples

``` r
# Calculate fp_auc of Sepal.Width as a classifier of setosa species
# in TPR = (0.9, 1)
fp_auc(iris, response = Species, predictor = Sepal.Width, lower_tpr = 0.9)
#> ℹ Upper threshold 1 already included in points.
#> • Skipping upper threshold interpolation
#> [1] 0.8516667
# Calculate np_auc of Sepal.Width as a classifier of setosa species
# in TPR = (0.9, 1)
np_auc(iris, response = Species, predictor = Sepal.Width, lower_tpr = 0.9)
#> ℹ Upper threshold 1 already included in points.
#> • Skipping upper threshold interpolation
#> [1] 0.472
```
