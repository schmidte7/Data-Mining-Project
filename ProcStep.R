#######################################################################
# 
# Fonction R permettant de faire du stepwise backward (method="backward")
# and forward (method="forward").
#
#######################################################################
# Last modified: 23.07.2012.
#######################################################################
# Warranty: none.
#######################################################################

ProcStep = function(matx, y, method="forward"){
  #
  if(is.na(match("package:leaps", search()))) require("leaps")
  #
  if(!(method=="forward"|method=="backward"))
    stop("This function works for stepwise 'backward' or 'forward' only!")
  #
  step.obj = regsubsets(matx, y, method=method, intercept=TRUE)
  #
  step.obj.sum = summary(step.obj)
  rss = step.obj.sum$rss
  outmat = step.obj.sum$outmat
  rownames(outmat) = 1:nrow(outmat)
  tmp.n = nrow(matx)
  p.size = apply(step.obj.sum$which, 1, sum)-1
  rss = c(summary(lm(y~1))$sigma^2*(tmp.n-1), step.obj.sum$rss)
  f.stat = numeric(nrow(outmat))
  for(i in 1:nrow(outmat)){
    f.stat[i] = ((rss[i]-rss[i+1])/rss[i+1])*(tmp.n-p.size[i]-1)
  }
  #
  if(step.obj$method=="forward"){
    sol = data.frame(outmat, Variables=p.size, RSS=rss[-1], F.value=f.stat)
    tmp.which = rep(" ", nrow(sol))
    tmp.opt.found = FALSE; j=1
    while(!tmp.opt.found) { if(sol$F.value[j] > 2) j=j+1
                            if(sol$F.value[j] <= 2) {
                              tmp.opt.found=TRUE
                              if(j==1) tmp.which[j] = "<----"
                              if(j>1) tmp.which[j-1] = "<----"
                          }
                          }
    sol = data.frame(sol, "Optimal"=tmp.which)                           
    rownames(sol) = 1:nrow(sol)
  }
  #
  if(step.obj$method=="backward"){
    outmatb = rbind(rep(" ",ncol(outmat)), outmat)
    rownames(outmatb) = 1:nrow(outmatb)
    sol = data.frame(outmatb, Variables=c(0, p.size), RSS=rss, F.value=c(f.stat,NA))
    tmp.which = rep(" ", nrow(sol))
    tmp.opt.found = FALSE; j=1
    while(!tmp.opt.found) { if(sol$F.value[j] > 2) j=j+1
                            if(sol$F.value[j] <= 2) {
                              tmp.opt.found=TRUE
                              tmp.which[j] = "<----"
                          }
                          }
    sol = data.frame(sol, "Optimal"=tmp.which)                               
    sol = sol[(nrow(sol)-1):1,]
    rownames(sol) = 1:nrow(sol)
  }
  sol
}

#######################################################################

