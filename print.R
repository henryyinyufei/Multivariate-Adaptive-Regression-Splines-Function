print = function(object, ...) UseMethod("print")
print.mars = function(object, ...) {
  cat("Call: " )
  print(object$call)
  cat("\n")
  cat("Coefficients: ")
  out = data.frame(object$coefficients)
  cat(unlist(out))
}

