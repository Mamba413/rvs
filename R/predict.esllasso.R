#' Make predictions from a "esllasso" object.
#'
#' Returns predictions from a fitted
#' "\code{esllasso}" object.
#'
#' @param object Output from the \code{esllasso} function.
#' @param newx New data used for prediction
#' @param ... Additional arguments affecting the predictions produced.
#' 
#' @return The predicted responses.
#' 
#' @export
#'
predict.esllasso <- function(object, newx, ...) {
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
