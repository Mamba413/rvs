#' Make predictions from a "rvs" object.
#'
#' Returns predictions from a fitted
#' "\code{rvs}" object.
#'
#' @param object Output from the \code{rvs} function.
#' @param newx New data used for prediction
#' @param ... Additional arguments affecting the predictions produced.
#' 
#' @return The predicted responses.
#' 
#' @export
#'
predict.rvs <- function(object, newx, ...) {
  if (missing(newx)) {
    stop("You need to supply a value for newx")
  }
  if (is.null(object[["alpha"]])) {
    newx <- as.matrix(newx)
    y <- newx %*% object[["beta"]]
  } else {
    newx <- as.matrix(newx)
    newx <- cbind(rep(1, nrow(newx)), newx)
    y <- newx %*% append(object[["alpha"]], object[["beta"]], after = 0)
  }
  return(y)
}
