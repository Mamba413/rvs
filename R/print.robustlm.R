#' Print method for a "robustlm" object
#'
#' Print the primary elements of the "\code{robustlm}" object.
#'
#' @param x A "\code{robustlm}" object.
#' @param ... Additional print arguments.
#' 
#' @return print a \code{robustlm} object.
#' 
#' @export
#'
print.robustlm <- function(x, ...){
  out <- list(
    beta = x[["beta"]],
    alpha = x[["alpha"]],
    gamma = x[["gamma"]],
    weight = x[["weight"]],
    loss = x[["loss"]]
  )
  print(out)
}
