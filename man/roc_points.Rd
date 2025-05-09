% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/roc_points.R
\name{roc_points}
\alias{roc_points}
\title{Calculate ROC curve points}
\usage{
roc_points(data = NULL, response, predictor, .condition = NULL)
}
\arguments{
\item{data}{A data.frame or extension (e.g. a tibble) containing values for
predictors and response variables.}

\item{response}{A data variable which must be a factor, integer or character
vector representing the prediction outcome on each observation
(\emph{Gold Standard}).

If the variable presents more than two possible outcomes, classes or
categories:
\itemize{
\item The outcome of interest (the one to be predicted) will remain distinct.
\item All other categories will be combined into a single category.
}

New combined category represents the "absence" of the condition to predict.
See \code{.condition} for more information.}

\item{predictor}{A data variable which must be numeric, representing values
of a classifier or predictor for each observation.}

\item{.condition}{A value from response that represents class, category or
condition of interest which wants to be predicted.

If \code{NULL}, condition of interest will be selected automatically depending on
\code{response} type.

Once the class of interest is selected, rest of them will be collapsed in a
common category, representing the "absence" of the condition to be predicted.

See \code{vignette("selecting-condition")} for further information on how
automatic selection is performed and details on selecting the condition of
interest.}
}
\value{
A tibble with two columns:
\itemize{
\item "tpr". Containing values for "true positive ratio", or y axis.
\item "fpr". Containing values for "false positive ratio", or x axis.
}
}
\description{
Calculates a series pairs of (FPR, TPR) which correspond to
points displayed by ROC curve. "false positive ratio" will be represented on
x axis, while "true positive ratio" on y one.
}
\examples{
# Calc ROC points of Sepal.Width as a classifier of setosa species
roc_points(iris, Species, Sepal.Width)
# Change class to predict to virginica
roc_points(iris, Species, Sepal.Width, .condition = "virginica")
}
