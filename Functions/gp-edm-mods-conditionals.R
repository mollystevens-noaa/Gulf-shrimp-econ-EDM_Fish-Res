##adapt getconditionals call to not limit length and plot against non-transformed CPUE

getconditionals2=function(fit,xrange="default", extrap=0.01, nvals=25, plot=T) {
  
  #need to add 2d option*****
  
  #extract relevant stuff from model
  iKVs=fit$covm$iKVs
  phi=fit$pars[grepl("phi",names(fit$pars))]
  sigma2=fit$pars[names(fit$pars)=="sigma2"]
  rho=fit$pars[names(fit$pars)=="rho"]
  rhomatrix=fit$inputs$rhomatrix
  X=fit$inputs$X
  Y=fit$inputs$Y
  Pop=fit$inputs$Pop
  scaling=fit$scaling$scaling
  ymeans=fit$scaling$ymeans
  ysds=fit$scaling$ysds
  xmeans=fit$scaling$xmeans
  xsds=fit$scaling$xsds
  
  if(xrange=="default") {
    if(scaling=="local") { xrange2="local" } 
    else { xrange2="global" }
  } else { xrange2=xrange }
  
  up=unique(Pop)
  np=length(up)
  d=ncol(X)
  Tslp=nvals
  
  outlist=NULL
  for(k in 1:np) { #populations
    indi=which(Pop==up[k])
    xval<-predmean<-predsd<-xg<-matrix(0,nrow=Tslp,ncol=d)
    poppred=rep(up[k],Tslp)
    for(i in 1:d) { #predictors
      if(xrange2=="local") {
        xgextrap=(max(X[indi,i])-min(X[indi,i]))*extrap
        xgi=seq(min(X[indi,i])-xgextrap,max(X[indi,i])+xgextrap,length.out = Tslp)
        #xgi=seq((1-extrap*sign(min(X[indi,i])))*min(X[indi,i]),(1+extrap*sign(max(X[indi,i])))*max(X[indi,i]),length.out = Tslp)
      } else {
        xgextrap=(max(X[,i])-min(X[,i]))*extrap
        xgi=seq(min(X[,i])-xgextrap,max(X[])+xgextrap,length.out = Tslp)
        #xgi=seq((1-extrap*sign(min(X[,i])))*min(X[,i]),(1+extrap*sign(max(X[,i])))*max(X[,i]),length.out = Tslp)
      }
      xp=xg
      xp[,i]=xgi
      covmnew=getcov(phi,sigma2,rho,X,xp,Pop,poppred,rhomatrix)
      Cs=covmnew$Cd #covariance matrix
      predmean[,i]=Cs%*%(iKVs%*%Y)
      predvar=numeric(length = Tslp)
      for(j in 1:Tslp) {
        predvar[j]=sigma2-Cs[j,]%*%iKVs%*%Cs[j,]
      }
      predsd[,i]=sqrt(predvar)
      xval[,i]=xgi
    }
    outlist[[k]]=list(poppred=poppred,xval=xval,predmean=predmean,predsd=predsd)
  }
  
  #unscale predictions and x
  if(scaling=="global") {
    for(i in 1:np) {
      for(j in 1:d) {
        outlist[[i]]$predmean[,j]=outlist[[i]]$predmean[,j]*ysds+ymeans
        outlist[[i]]$predsd[,j]=sqrt(outlist[[i]]$predsd[,j]^2*ysds^2)
        outlist[[i]]$xval[,j]=outlist[[i]]$xval[,j]*xsds[j]+xmeans[j]
      }
    }
  }
  if(scaling=="local") {
    for(i in 1:np) {
      for(j in 1:d) {
        locmean=ymeans[as.character(up[i])==names(ymeans)]
        locsd=ysds[as.character(up[i])==names(ysds)]
        outlist[[i]]$predmean[,j]=outlist[[i]]$predmean[,j]*locsd+locmean
        outlist[[i]]$predsd[,j]=sqrt(outlist[[i]]$predsd[,j]^2*locsd^2)
        locmean=xmeans[[which(as.character(up[i])==names(xmeans))]][j]
        locsd=xsds[[which(as.character(up[i])==names(xsds))]][j]
        outlist[[i]]$xval[,j]=outlist[[i]]$xval[,j]*locsd+locmean
      }
    }
  }
  
  #add back in linear prior
  if(!is.null(fit$linprior)) {
    for(i in 1:np) {
      regdf_new=data.frame(x=outlist[[i]]$xval[,1],pop=outlist[[i]]$poppred)
      ynewlinprior=predict(fit$linprior$linprior_reg, newdata=regdf_new)
      outlist[[i]]$predmean[,1]=outlist[[i]]$predmean[,1]+ynewlinprior
      if(d>1) {
        for(j in 2:d) {
          if(scaling=="global") locmean1=xmeans[1]
          if(scaling=="local") locmean1=xmeans[[which(as.character(up[i])==names(xmeans))]][1]
          if(scaling=="none") locmean1=0
          regdf_new=data.frame(x=rep(locmean1,nvals),pop=outlist[[i]]$poppred)
          ynewlinprior=predict(fit$linprior$linprior_reg, newdata=regdf_new)
          outlist[[i]]$predmean[,j]=outlist[[i]]$predmean[,j]+ynewlinprior
        }
      }
    }
  }
  
  if(!is.null(fit$inputs$x_names2)) { xlabels=fit$inputs$x_names2 } 
  else if(!is.null(fit$inputs$x_names)) { xlabels=fit$inputs$x_names }
  else { xlabels= paste0("x",1:d) }
  
  #combine into dataframe  #this needs work, put in longer format*****
  out=lapply(outlist,function(x) {
    cc2=cbind.data.frame(x$poppred,x$xval,x$predmean,x$predsd)
    colnames(cc2)=c("pop",xlabels,paste0(xlabels,"_yMean"),paste0(xlabels,"_ySD"))
    return(cc2)
  })
  out=do.call("rbind",out)
  
  if(plot) {
    if(!is.null(fit$inputs$y_names)) {
      yl=fit$inputs$y_names
      if(!is.null(fit$b)) {
        if(fit$inputs$ytrans=="none") {yl=sub("_trans","",yl)}
      }
    } else {
      yl="y"
    }
    old.par <- par(no.readonly = TRUE)
    #old.par <- par(mfrow=c(min(4,np),min(4,d)),mar=c(5,4,2,2))
    on.exit(par(old.par),add = T,after = F)
    par(mfrow=c(min(4,np),min(4,d)),mar=c(5,4,2,2))
    
    for(i in 1:np) {
      ylims=range(out[out$pop==up[i],grep("_yMean",colnames(out))]+out[out$pop==up[i],grep("_ySD",colnames(out))],
                  out[out$pop==up[i],grep("_yMean",colnames(out))]-out[out$pop==up[i],grep("_ySD",colnames(out))])
      pdata=outlist[[i]]
      for(j in 1:d) {
        plot(pdata$xval[,j],pdata$predmean[,j], type="l",xlab=xlabels[j],ylab=yl,main=up[i],ylim=ylims)
        #ylim=range(pdata$predmean[,j]+pdata$predsd[,j],pdata$predmean[,j]-pdata$predsd[,j]))
        lines(pdata$xval[,j],pdata$predmean[,j]+pdata$predsd[,j],lty=2)
        lines(pdata$xval[,j],pdata$predmean[,j]-pdata$predsd[,j],lty=2)
      }
    }
  }
  return(invisible(out))
}

