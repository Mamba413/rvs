#' Print method for a "esllasso" object
#'
#' Print the primary elements of the "\code{esllasso}" object.
#'
#' @param x A "\code{esllasso}" object.
#' @param ... Additional print arguments.
#' 
#' @return print a \code{esllasso} object.
#' 
#' @export
#'
print.esllasso <- function(x, ...){
  out <- list(
    beta = x[["beta"]],
    alpha = x[["alpha"]],
    gamma = x[["gamma"]],
    weight = x[["weight"]],
    loss = x[["loss"]]
  )
  print(out)
}
