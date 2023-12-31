% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qenv-join.R
\name{join}
\alias{join}
\alias{join,qenv,qenv-method}
\alias{join,qenv.error,ANY-method}
\alias{join,qenv,qenv.error-method}
\title{Join two \code{qenv} objects}
\usage{
join(x, y)

\S4method{join}{qenv,qenv}(x, y)

\S4method{join}{qenv.error,ANY}(x, y)

\S4method{join}{qenv,qenv.error}(x, y)
}
\arguments{
\item{x}{(\code{qenv})}

\item{y}{(\code{qenv})}
}
\value{
\code{qenv} object.
}
\description{
\code{join()} perform checks and merges two \code{qenv} objects into one \code{qenv} object.
Any common code at the start of the \code{qenvs} is only placed once at the start of the joined \code{qenv}.
This allows consistent behavior when joining \code{qenvs} which share a common ancestor.
See below for an example.
}
\details{
There are some situations where \code{join()} cannot be properly performed, such as these three scenarios:
\enumerate{
\item Both \code{qenv} objects contain an object of the same name but are not identical. \cr\cr
Example:
\preformatted{
  x <- new_qenv(
    code = c(mtcars1 = "mtcars1 <- mtcars"),
    env = list2env(list(mtcars1 = mtcars))
  )
  y <- new_qenv(
    code = c(mtcars1 = "mtcars1 <- mtcars['wt']"),
    env = list2env(list(mtcars1 = mtcars['wt']))
  )
  z <- join(x, y)
  # Error message will occur
  }
In this example, \code{mtcars1} object exists in both \code{x} and \code{y} objects but the content are not identical.\cr
\code{mtcars1} in the \verb{x qenv} object has more columns than \code{mtcars1} in the \verb{y qenv} object (only has one column).
\item \code{join()} will look for identical \verb{@id} values in both \code{qenv} objects.
The index position of these \verb{@id}s must be the same to determine the evaluation order.
Otherwise, \code{join()} will throw an error message.\cr\cr
Example:
\preformatted{
  common_q <- new_qenv(code = "v <- 1", env = list2env(list(v = 1)))
  x <- eval_code(
    common_q,
    "x <- v"
  )
  y <- eval_code(
    common_q,
    "y <- v"
  )
  z <- eval_code(
    y,
    "z <- v"
  )
  q <- join(x, y)
  join_q <- join(q, z)
  # Error message will occur

  # Check the order of evaluation based on the id slot
  shared_ids <- intersect(q@id, z@id)
  match(shared_ids, q@id) # Output: 1 3
  match(shared_ids, z@id) # Output: 1 2
  }
The error occurs because the index position of identical \verb{@id} between the two objects is not the same.
\item The usage of temporary variable in the code expression could cause \code{join()} to fail. \cr\cr
Example:
\preformatted{
  common_q <- new_qenv()
  x <- eval_code(
    common_q,
    "x <- numeric(0)
     for (i in 1:2) {
       x <- c(x, i)
     }"
  )
  y <- eval_code(
    common_q,
    "y <- numeric(0)
     for (i in 1:3) {
       y <- c(y, i)
     }"
  )
  q <- join(x,y)
  # Error message will occur

  # Check the value of temporary variable i in both objects
  x@env$i # Output: 2
  y@env$i # Output: 3
  }
\code{join()} fails to provide a proper result because of the temporary variable \code{i} exists
in both objects but has different value.\cr
To fix this, we can set \code{i <- NULL} in the code expression for both objects.
\preformatted{
  common_q <- new_qenv()
  x <- eval_code(
    common_q,
    "x <- numeric(0)
     for (i in 1:2) {
       x <- c(x, i)
     }
     # dummy i variable to fix it
     i <- NULL"
  )
  y <- eval_code(
    common_q,
    "y <- numeric(0)
     for (i in 1:3) {
       y <- c(y, i)
     }
     # dummy i variable to fix it
     i <- NULL"
  )
  q <- join(x,y)
  }
}
}
\examples{
q1 <- new_qenv(
  code = c(iris1 = "iris1 <- iris", mtcars1 = "mtcars1 <- mtcars"),
  env = list2env(list(
    iris1 = iris,
    mtcars1 = mtcars
  ))
)
q2 <- q1
q1 <- eval_code(q1, "iris2 <- iris")
q2 <- eval_code(q2, "mtcars2 <- mtcars")
qq <- join(q1, q2)
get_code(qq)

common_q <- new_qenv(list2env(list(x = 1)), quote(x <- 1))
y_q <- eval_code(common_q, quote(y <- x * 2))
z_q <- eval_code(common_q, quote(z <- x * 3))
join_q <- join(y_q, z_q)
# get_code only has "x <- 1" occurring once
get_code(join_q)

}
