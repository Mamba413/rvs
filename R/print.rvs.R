#' Print method for a "rvs" object
#'
#' Print the primary elements of the "\code{rvs}" object.
#'
#' @param x A "\code{rvs}" object.
#' @param ... Additional print arguments.
#' 
#' @return print a \code{rvs} object.
#' 
#' @export
#'
print.rvs <- function(x, ...){
  out <- list(
    beta = x[["beta"]],
    alpha = x[["alpha"]],
    gamma = x[["gamma"]],
    weight = x[["weight"]],
    loss = x[["loss"]]
  )
  print(out)
}
