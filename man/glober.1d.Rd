\name{glober.1d}
\alias{glober.1d}
\title{ Estimation of functions with one input variable
}
\description{
This function implements the method described in Savino, M and Levy-Leduc, C (2023) for estimating functions in the one-dimensional setting with observations which are assumed to satisfy a nonparametric regression model. The observation points belong to a compact set of \eqn{\mathbb{R}}.  

}
\usage{
glober.1d(x, y, xpred, ord, parallel = FALSE, nb.Cores = 1)
}
\arguments{
  \item{x}{
matrix of one column or vector containing the values of the input variables.
}
  \item{y}{
vector containing the corresponding response variable associated to the input values \eqn{\texttt{x}}.
}
  \item{xpred}{
matrix of one column or vector containing the input variables for which \eqn{f} has to be estimated.
}
  \item{ord}{
order of the B-spline basis used in the regression model. Default is 3 (quadratic B-splines).
}
  \item{parallel}{
  logical, if TRUE then a parallelized version of the code is used. Default is FALSE. 
}
  \item{nb.Cores}{
 numerical, number of cores used for parallelization, if parallel is set to TRUE.
}
}
\value{
\item{festimated}{estimation of \eqn{f} at \eqn{\texttt{xpred}}.}
\item{knotSelec}{selected knots used in the definition of the B-splines.}
\item{rss}{residual sum-of-squares (RSS) of the model.}
\item{rsq}{R-squared of the model, calculated as \eqn{1 - \frac{RSS}{TSS}} where TSS is the total sum-of-squares of the model. }
}
\examples{
# --- Loading values of x --- #
data('x_1D')
# --- Loading values of the corresponding y --- #
data('y_1D')
# --- Loading values of xpred --- #
data('xpred_1D')

# --- Estimation of f at xpred --- #
glober.1d(x = x_1D, y = y_1D, xpred = xpred_1D, ord = 3, parallel = FALSE)

\donttest{
# --- Parallel computing --- #
glober.1d(x = x_1D, y = y_1D, xpred = xpred_1D, ord = 3, parallel = TRUE, nb.Cores = 2)
} 
}

