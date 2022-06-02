anova = function(object,...) UseMethod("anova")
anova.mars = function(object,...) {
  rss = sum(residuals(object)*residuals(object))
  N = length(fitted(object))
  m = length(object$coefficients) - 1
  gcv = rss * N / ((N - m - 3)*(N - m - 3))
  print(m)
  print(paste("GCG is: ", gcv, " RSS is: ", rss), quote = FALSE)
}
  
  
  

  
