##GP-EDM FUNCTIONS TO MODIFY

##Grab start year from max embedding dimension ; re-run R2 metrics
##Check accuracy for max embedding outputs from GP-EDM on R2 metrics



getR2=function(obs, pred) {
  d=na.omit(cbind(obs, pred))
  R2=1-sum((d[,1]-d[,2])^2)/sum((d[,1]-mean(d[,1]))^2)
  return(R2)
}

#' Calculate R-squared by population (and other stuff)
#'
#' Calculates R-squared values for each population. Can also compute modified 
#' total R-squared values, with data centered and/or scaled within populations.
#' This can provide a more useful total fit estimate if the populations have
#' very different local means.
#' 
#' @details
#' Returned R-squared might be negative. This indicates that the prediction is 
#' worse than using the mean.
#'
#' @param obs Vector of observed values.
#' @param pred Vector of predicted values.
#' @param pop Vector of pop identifiers.
#' @param type The type of R-squared to calculate: \code{"within"} (default) gets
#'   within-population, \code{"centered"} gets across-population but with local means removed,
#'   \code{"scaled"} get across-population but with local scaling (local means removed and 
#'   rescaled to unit variance).
#' @return If \code{type="within"}, a list of 2 named vectors with R-squared and rmse for
#'   each populaiton. Otherwise a scalar (the R-squared).
#' @seealso \code{\link{getR2}}
#' @export
#' @keywords functions
getR2pop=function(obs, pred, pop, type=c("within","centered","scaled")) {
  type=match.arg(type)
  up=unique(pop)
  np=length(up)
  if(type=="within") {
    R2pop<-rmsepop<-numeric(np)
    names(R2pop)=up
    names(rmsepop)=up
    for(p in 1:np) {
      ind=which(pop==up[p])
      R2pop[p]=getR2(obs[ind],pred[ind])
      rmsepop[p]=sqrt(mean((obs[ind]-pred[ind])^2,na.rm=T))
    }
    insampfitstatspop=list(R2pop=R2pop,rmsepop=rmsepop)
    return(insampfitstatspop)
  }
  if(type=="centered") {
    ymeans=tapply(obs,pop,mean,na.rm=T)
    obs_s=obs
    pred_s=pred
    for(p in 1:length(up)) {
      locmean=ymeans[as.character(up[p])==names(ymeans)]
      obs_s[pop==up[p]]=(obs_s[pop==up[p]]-locmean)
      pred_s[pop==up[p]]=(pred_s[pop==up[p]]-locmean)
    }
    R2=getR2(obs_s, pred_s)
    return(R2)
  }
  if(type=="scaled") {
    ymeans=tapply(obs,pop,mean,na.rm=T)
    ysds=tapply(obs,pop,sd,na.rm=T)
    obs_s=obs
    pred_s=pred
    for(p in 1:length(up)) {
      locmean=ymeans[as.character(up[p])==names(ymeans)]
      locsd=ysds[as.character(up[p])==names(ysds)]
      obs_s[pop==up[p]]=(obs_s[pop==up[p]]-locmean)/locsd
      pred_s[pop==up[p]]=(pred_s[pop==up[p]]-locmean)/locsd
    }
    R2=getR2(obs_s, pred_s)
    return(R2)
  }
}
