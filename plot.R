plot = function(object,...) UseMethod("plot")
plot.mars = function(object,...) {
  
  #Residuals vs Fitted
  plot.default(object$fitted.values, object$residuals, 
               xlab = "Fitted", ylab = "Residuals", 
               main = "Residuals vs Fitted", pch=16)
  abline(h = 0, col = 'red')
}