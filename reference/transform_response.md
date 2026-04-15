# Transforms a response variable into a valid factor that can be processed downstream.

`transform_response` transforms response so that it can be processed in
further steps. Function transforms input into a `factor` of values 1 and
0 corresponding to the condition of interest and absence of it
respectively.

## Usage

``` r
transform_response(response, .condition = NULL)
```

## Arguments

- response:

  A factor, integer or character vector of categories.

## Value

`factor` of levels `(0,1)`, where 1 represents the condition of interest
and 0 absence of it.

## Details

By default function takes some assumption on how to make transformation,
depending on the class of `response`:

- factor. Function considers the condition of interest first level in
  factor.

- integer. Function considers the condition of interest the `min` value
  of\\ response.

- character. Function considers the condition of interest the first
  value in `unique(response)` after using `sort`.
