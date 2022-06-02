library(earth)
library(rpart)

###############################
##########   mars   ###########
###############################
mars = function(formula,data,control=NULL,...) {
  cc = match.call()
  mf = model.frame(formula,data)
  y = model.response(mf)
  mt = attr(mf,"terms")
  x = model.matrix(mt,mf)
  if(is.null(control)) control = mars.control()
  fwd = fwd_selection(y,x,control)
  bwd = bwd_selection(fwd,control)
  fit = lm(y~.-1,data=data.frame(y=y,bwd$B))
  out = c(list(cc,GCV=bwd$GCV,y=y,B=bwd$B,splits=bwd$splits,formula=formula,x=x,control=control),fit)
  class(out) = c("mars",class(fit))
  out
}

#################################
######   fwd_selection   ########
#################################
fwd_selection = function(y,x,control=mars.control()) {
  N = length(y)
  n = ncol(x)
  B = matrix(1,nrow=N,ncol=1)
  splits = list(data.frame(m=0,v=0,s=NA,t=NA))
  M = 1
  
  while(!(M>control$Mmax)) {
    if(control$trace) cat("Max Basis",M,"\n")
    lof_best = Inf
    
    for(m in 1:M) {

      vv = setdiff(1:n,splits[[m]]$v) 

      if(control$trace) cat("Max Basis =",M,"mth basis =",m,"var =",vv,"\n")
      
      for(v in vv) {
        tt = split_points(x[,v],B[,m])
        
        for(t in tt) {

          Bnew = data.frame(B,
                            Btem1=B[,m]*h(x[,v],+1,t),
                            Btem2=B[,m]*h(x[,v],-1,t))
          gdat = data.frame(y=y,Bnew)
          lof = LOF(y~.,gdat,control)
          if(lof < lof_best) {
            lof_best=lof; m_best=m; v_best=v; t_best=t
          }
          
        }
        
      }
      
    }
    
    right_df = rbind(splits[[m_best]],c(m_best,v_best,+1,t_best))
    left_df = rbind(splits[[m_best]],c(m_best,v_best,-1,t_best))
    
    splits = c(splits,list(right_df),list(left_df))
    
    B = cbind(B,
              B[,m_best]*h(x[,v_best],+1,t_best),
              B[,m_best]*h(x[,v_best],-1,t_best))
    
    M = M + 2
  }
  
  colnames(B) = paste0("B",(0:(ncol(B)-1)))
  return(list(y=y,B=B,splits=splits))
}

#################################
######   bwd_selection   ########
#################################
bwd_selection = function(fwd,control) {
  Mmax = ncol(fwd$B)
  Jbest = 2:Mmax
  Kbest = Jbest
  gdat = data.frame(y=fwd$y,fwd$B)
  lofbest = LOF(y~.,gdat,control)
  
  for(M in Mmax:2) {
    L = Kbest
    b = Inf

    if(control$trace) cat("Basis # (except for intercept)",L,"\n")
    
    for(m in L) {
      K = setdiff(L,m)
      gdat2 = data.frame(y=fwd$y,fwd$B[,K])
      lof = LOF(y~., gdat2, control)
      if(control$trace) cat("Max Basis:",M,"Remained Basis #:",K,"LOF value =",lof,"\n")
      if(lof<b){
        b=lof; Kbest=K
      }
      if(lof<lofbest) {
        lofbest = lof; Jbest = K
      }
      
    }
    
  }
  
  Jbest=c(1,Jbest)
  return(list(y=fwd$y,B=fwd$B[,Jbest],splits=fwd$splits[Jbest],GCV=lofbest))
}



####################################
####### SUPPORTING FUNCTIONS #######
####################################
LOF = function(form,data,control) { 
  ff = lm(form,data) 
  N = nrow(data)
  M = ncol(data)-1
  C_M = sum(hatvalues(ff))
  C_M_tilde = C_M + (control$d*M)
  GCV = sum(residuals(ff)^2) * (N/((N-C_M_tilde)^2))
  return(GCV)
}

h = function(x,s,t) {
  return(pmax(0,s*(x-t)))
}

split_points = function(xv,Bm) {
  out = sort(unique(xv[Bm>0]))
  return(out[-length(out)])
}



##############################
####### mars.control() #######
##############################
new_mars.control = function(Mmax,d,trace) {
  structure(list(Mmax=Mmax,d=d,trace=trace),class="mars.control")
}
validate_mars.control = function(control) {
  if(control$Mmax < 2) {
    warning("Mmax must be equal to or greater than 2; setting to 2")
    control$Mmax = 2
  }
  control
}
mars.control = function(Mmax=2,d=3,trace=FALSE) {
  control = new_mars.control(Mmax,d,trace)
  validate_mars.control(control)
}
