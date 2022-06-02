predict = function(object, newdata=NULL, ...) UseMethod("predict")
predict.mars = function(object,newdata=NULL, ...) {
  
  if(missing(newdata) || is.null(newdata)) {
    return(object$fitted.values)
  }
  
  else {
    tt = terms(object$formula)
    tt = delete.response(tt)
    mf = model.frame(tt,newdata)
    mt = attr(mf,"terms")
    X = model.matrix(mt,mf)
    
    sp_mod = lapply(object$splits, function(x) x[-1,])
    
    f = function(x) {
      temp_b = matrix(ncol=nrow(x), nrow=nrow(newdata))
      new_b = matrix(ncol=1, nrow=nrow(newdata))
      nm = colnames(object$x)[x$v]
      nd = as.matrix(X[,nm])
      for (i in 1:nrow(x)) {
        temp_b[,i] = h(x=nd[,i],s=x$s[i],t=x$t[i])
      }
      for(i in 1:nrow(newdata)) {
        new_b[i,] = prod(temp_b[i,])
      }
      return(new_b)
    }
    
    B_list = lapply(sp_mod[-1], f)
    
    B_list = c(list(matrix(rep(1,times=nrow(newdata)),ncol=1)), B_list)
    
    B_df = as.data.frame(do.call(cbind, B_list))
    
    coef = object$coefficients[!is.na(object$coefficients)]
    
    return(rowSums(t(t(B_df)*coef)))
    
  }
}