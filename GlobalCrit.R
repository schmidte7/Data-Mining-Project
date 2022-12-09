#######################################################################
# 
# Fonction R permettant de faire des methodes globales pour la
# selection de modeles.
#
#######################################################################
# Last modified: 10.09.2013.
#######################################################################
# Warranty: none.
#######################################################################

GlobalCrit = function(obj, crit=NULL, best=10, aic="nosimpl"){
	# obj    : output de lm() ou de lmRob()
	# crit   : defaut=NA implique critere classique ou robuste de selon obj 
	#          autre possibilites: "all", "AIC", "RAIC", "Cp"
	# best   : nombre de meilleurs modeles selectionnes pour chaque critere, par defaut 10.
	# aic    : maniere de calculer l'AIC  
	#         "nosimpl"=sans simplification,"simpl"=avec simplification (p.r a stepAIC dans MASS)
	objrob=sum(obj$M.weights)>0
  matxdata=get(as.character(obj$call$data))
	n=dim(matxdata)[1]
	varxy=dimnames(attr(obj$terms,"factors"))[[1]]
	y=varxy[1]
	X=varxy[2:length(varxy)]
	p=length(X)
	#
	matxposs=matrix(NA,nrow=2^p,ncol=p)
	for(i in 1:p){matxposs[,i]=rep(c(rep(1,2^(p-i)),rep(0,2^(p-i))),2^(i-1))}
	nreg=(2^p)-1
	best=min(nreg,best)
	matxposs=matxposs[1:nreg,]
	#
	critposs=c("Cp","AIC","RAIC")
	if(is.null(crit)){
		if(objrob){critwanted=c(0,0,1)}else{
			        critwanted=c(1,1,0)}
		}else{
	if(crit=="all"){critwanted=c(1,1,1)
		}else{
	if(crit=="Cp"){critwanted=c(1,0,0)
		}else{
	if(crit=="AIC"){critwanted=c(0,1,0)
		}else{
	if(crit=="RAIC"){critwanted=c(0,0,1)}
	}}}}
	ncrit=sum(critwanted)
	#
	vectformula=rep(NA,2^p)
	for(i in 1:nreg){vectformula[i]=paste(y,paste(X[matxposs[i,]==1],collapse="+"),sep="~")}
	matxres=matrix(NA,ncol=3,nrow=nreg)
	if(sum(critwanted[1:2])>0){
		reglscompl=lm(as.formula(vectformula[1]),data=matxdata)
		sigcomplet=sum(reglscompl$residuals^2)/reglscompl$df.resid
		if(aic=="nosimpl"){
			for(i in 1:nreg){
				rss=sum(residuals(lm(as.formula(vectformula[i]),data=matxdata))^2)
				matxres[i,2]=n+n*log(2*pi)+n*log(rss/n)+2*(sum(matxposs[i,])+2)
				matxres[i,1]=rss/sigcomplet+2*(sum(matxposs[i,])+1)-n
				}
			}else{
			for(i in 1:nreg){
				rss=sum(residuals(lm(as.formula(vectformula[i]),data=matxdata))^2)
				matxres[i,2]=n*log(rss/n)+2*(sum(matxposs[i,])+1)
				matxres[i,1]=rss/sigcomplet+2*(sum(matxposs[i,])+1)-n
				}
			}
		}
	if(critwanted[3]>0){
		#
		lmRob.RAIC=function(object, scale = NULL){
			if(object$est == "initial")	warning("Inference based on initial estimates is not recommended.")
			tmp <- object$robust.control$final.alg
			if(tmp == "Adaptive" || tmp == "adaptive") stop("RAIC is only available for final MM-estimates.")
			p <- length(object$coef)
			if(is.null(scale)) scale <- object$scale
			res <- object$residuals/scale
			psif <- object$robust.control$weight
			efficiency <- object$robust.control$efficiency
			if(casefold(psif[2]) == "optimal") ipsi <- 1
			else ipsi <- 2
			yc <- object$yc
			a <- sum(rho.weight(res, ipsi, yc))
			b <- p * sum(psi.weight(res, ipsi, yc)^2)
			d <- sum(psp.weight(res, ipsi, yc))
			RAIC = a + b/d
			if(RAIC  < 0) return(NA)
			RAIC 
			}
		if(objrob){efficiencywanted=obj$robust.control$efficiency}else{
			efficiencywanted=.9}
		sigmacomplet=lmRob(as.formula(vectformula[1]),data=matxdata,
						           control=lmRob.control(efficiency=efficiencywanted,
                                             trace=FALSE,
                                             mxr=1000, mxf=1000, mxs=1000))$scale
		for(i in 1:nreg){
			regMM=lmRob(formula=as.formula(vectformula[i]),data=matxdata,
						control=lmRob.control(efficiency=efficiencywanted, trace=FALSE,
                                             mxr=1000, mxf=1000, mxs=1000))
			matxres[i,3]=lmRob.RAIC(regMM,scale=sigmacomplet)
			}
		}
	### -> prepare output
	colcrit=seq(1,3)[critwanted==1]
	matxrank=matxres
	if(ncrit>1){matxrank[,colcrit]=apply(matxres[,colcrit],2,rank)}else{
		matxrank[,colcrit]=rank(matxres[,colcrit])}
	selectsmallest=function(matx,best,colcrit,ncrit,nreg){
		matx2=matrix(NA,nrow=best,ncol=3)
		for(i in 1:ncrit){
			matx2[,colcrit[i]]=seq(1,nreg)[matx[,colcrit[i]]<=best]
			}
		as.numeric(names(table(matx2)))
		}
	givename.model=function(vect.matxposs,p){
		paste(c(LETTERS,letters)[seq(1,p)[vect.matxposs==1]],collapse="")
		}
	coordselectmodels=selectsmallest(matxrank,best,colcrit,ncrit,nreg)
	nmodels=length(coordselectmodels)
	bigoutput=as.data.frame(cbind(matxres,
	                                  matxrank,
	                                  apply(matxposs,1,sum),
	                                  apply(matxposs,1,givename.model,p),
	                                  matxposs))
  dimnames(bigoutput)[[2]]=c("Cp","AIC","RAIC","rankCp","rankAIC","rankRAIC","p-1","model",
	                               c(LETTERS,letters)[1:p])   
	smalloutput=bigoutput[coordselectmodels,]
	bigoutput=bigoutput[order(bigoutput$"p-1"),]
	smalloutput=smalloutput[order(smalloutput$"p-1"),]
	### -> output
	output=list(smalloutput=smalloutput,bigoutput=bigoutput,
				  critwanted=critwanted,ncrit=ncrit,colcrit=colcrit,nmodels=nmodels,
				  n=n,y=y,X=X,p=p,critposs=critposs,dataset=obj$call$data,best=best
				 )
	class(output)="GlobalCrit"
	print(output)
	output
	}

print.GlobalCrit=function(obj){
	options(width=111)
	obj$smalloutput[,1:7]=apply(apply(obj$smalloutput[,1:7],2,as.character),2,as.numeric)
	### -> sous-fonctions utiles...
	ncharControl.fun=function(x=NA,digit=3){
            nchar.z=5+digit+1
            bornes.char=NA
            z=paste("     .000000000000000000")
            z=substring(z,1,nchar.z)   
            y=abs(x)
            nchar.inty=nchar(as.integer(y))
            if(x<0){substring(z,1,1)="-"}
            substring(z,(5-nchar.inty+1),5)=as.character(as.integer(y))
			  apres=substring("0000",1,digit)
			  apres2=substring(as.character(round(y-as.integer(y),digit)),3,nchar.z-2)
			  substring(apres,1,nchar(apres2))=apres2
            substring(z,7,nchar.z)=apres
            z
            }
	ncharControl2.fun=function(x=NA,digit=3){
            z="  "
            if(nchar(x)==1){substring(z,2,2)=as.character(x)}else{
	           z=as.character(x)}
            z
            }
	ncharImpose.fun=function(x,n=NA,begin=T){
            x=as.character(x)
            nchar.x=nchar(x)
            z="                                    "
            z=substring(z,1,n)
            if(begin){substring(z,1,nchar.x)=x}else{
                      substring(z,(n-nchar.x+1),n)=x}
            z
            }
	## info suppl
	pdetraitdesplus="-----------------------------------------------------------------------------------------------------------------"
	max.namesmodel=max(c(nchar(as.vector(obj$smalloutput$model)),10))
	names.crit=obj$critposs[seq(1,3)[obj$critwanted==1]]
	if(obj$ncrit==1){pdetrait=substring(pdetraitdesplus,1,max.namesmodel+3+13+1+3)}else{
		if(obj$ncrit==2){pdetrait=substring(pdetraitdesplus,1,max.namesmodel+2*3+2*13+2+5)}else{
			if(obj$ncrit==3){pdetrait=substring(pdetraitdesplus,1,max.namesmodel+3*3+2*14+13+2+7)}}}
	if(obj$ncrit==1){pdetrait2=substring(pdetraitdesplus,1,max.namesmodel+3+13+1+3+4+5)}else{
		if(obj$ncrit==2){pdetrait2=substring(pdetraitdesplus,1,max.namesmodel+2*3+2*13+2+5+4)}else{
			if(obj$ncrit==3){pdetrait2=substring(pdetraitdesplus,1,max.namesmodel+3*3+2*14+13+2+7+4)}}}
	## print output
	cat("\n")
	cat(pdetrait2,"\n")
	# cat(      " PROCEDURE GLOBALE DE SELECTION DE VARIABLES \n\n")
	cat(      "  GLOBAL VARIABLE SELECTION PROCEDURE \n\n")
	cat(      "  ( Data = ",obj$dataset,")\n")
	cat("\n")
	for(aaa in 1:obj$p){
		cat(" ",c(LETTERS,letters)[aaa],"=",ncharImpose.fun(obj$X[aaa],16),"\n")
		}
	cat("\n")
	if(obj$ncrit==1){
		cat(" ",ncharImpose.fun("Models",max.namesmodel),
	    " | ",ncharImpose.fun(names.crit[1],13),"|\n")}else{
	if(obj$ncrit==2){
		cat(" ",ncharImpose.fun("Models",max.namesmodel),
	    " | ",ncharImpose.fun(names.crit[1],13),
	    " | ",ncharImpose.fun(names.crit[2],13)," |\n")}else{
	if(obj$ncrit==3){
		cat(" ",ncharImpose.fun("Models",max.namesmodel),
	    " | ",ncharImpose.fun(names.crit[1],14),
	    " | ",ncharImpose.fun(names.crit[2],14),
	    " | ",ncharImpose.fun(names.crit[3],13)," |\n")}}}
	cat(" ",pdetrait,"\n")
	for(i in 1:obj$nmodels){
		if(obj$ncrit==1){
			cat(" ",ncharImpose.fun(obj$smalloutput$model[i],max.namesmodel),
		    " | ",ncharControl.fun(obj$smalloutput[i,obj$colcrit[1]],2),
			if(obj$smalloutput[i,obj$colcrit[1]+3]>obj$best){"      | \n"}else{
			paste(" (",ncharControl2.fun(obj$smalloutput[i,obj$colcrit[1]+3]),")|\n",sep="")})
		   }
		if(obj$ncrit==2){
			cat(" ",ncharImpose.fun(obj$smalloutput$model[i],max.namesmodel),
		    " | ",ncharControl.fun(obj$smalloutput[i,obj$colcrit[1]],2),
			if(obj$smalloutput[i,obj$colcrit[1]+3]>obj$best){"     "}else{
			paste(" (",ncharControl2.fun(obj$smalloutput[i,obj$colcrit[1]+3]),")",sep="")},
			  "| ",ncharControl.fun(obj$smalloutput[i,obj$colcrit[2]],2),
			if(obj$smalloutput[i,obj$colcrit[2]+3]>obj$best){"      | \n"}else{
			paste(" (",ncharControl2.fun(obj$smalloutput[i,obj$colcrit[2]+3]),") |\n",sep="")})		
			}
		if(obj$ncrit==3){
			cat(" ",ncharImpose.fun(obj$smalloutput$model[i],max.namesmodel),
		    " | ",ncharControl.fun(obj$smalloutput[i,obj$colcrit[1]],2),
			if(obj$smalloutput[i,obj$colcrit[1]+3]>obj$best){"     "}else{
			paste(" (",ncharControl2.fun(obj$smalloutput[i,obj$colcrit[1]+3]),")",sep="")},
			  " | ",ncharControl.fun(obj$smalloutput[i,obj$colcrit[2]],2),
			if(obj$smalloutput[i,obj$colcrit[2]+3]>obj$best){"     "}else{
			paste(" (",ncharControl2.fun(obj$smalloutput[i,obj$colcrit[2]+3]),")",sep="")},
			  " | ",ncharControl.fun(obj$smalloutput[i,obj$colcrit[3]],2),
			if(obj$smalloutput[i,obj$colcrit[3]+3]>obj$best){"      | \n"}else{
			paste(" (",ncharControl2.fun(obj$smalloutput[i,obj$colcrit[3]+3]),") |\n",sep="")})				
			}
		}
		cat("\n")
		cat(pdetrait2,"\n")
	}

#######################################################################
