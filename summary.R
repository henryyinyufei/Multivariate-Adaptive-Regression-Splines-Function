summary = function(object, ...) UseMethod("summary")
summary.mars = function(object, ...) {
  out = list()
  
  new_split = lapply(object$splits, function(x) x[-1,])
  
  f_hinge = function(x){
    h_form = paste("h(",x$s,"(",colnames(object$x)[x$v],"-",x$t,"))",sep="")
    paste(h_form,collapse="")
  }
  h_split = lapply(new_split, f_hinge)
  
  coefficients = object$coefficients
  cf_table = data.frame(coefficients=coefficients)
  rownames(cf_table) = c("intercept",unlist(h_split)[-1])
  out$coefficients = cf_table
  
  return(out)
}