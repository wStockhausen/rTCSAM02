#'
#' @title Extract OFL results from an mcmc run to a dataframe
#'
#' @description Function to extract OFL results from an mcmc run to a dataframe
#'
#' @param mcmc - mcmc list from TCSAM02 mdoel run
#'
#' @return dataframe with
#'
#' @details none.
#'
#' @export
#'
mcmc.ExtractOFLResults<-function(mcmc){
  n<-length(mcmc);
  nms<-c("objFun","OFL","Fofl","prjB","curB","Fmsy","Bmsy","MSY","B100","avgRecM","avgRecF");
  v<-matrix(nrow=n,ncol=length(nms));
  colnames(v)<-nms;
  for (i in 1:n){
    if (!is.null(mcmc[[i]][["objFun"]])) {v[i,"objFun"]<-mcmc[[i]][["objFun"]];}
    for (nm in nms[2:length(nms)]){
      if(!is.null(mcmc[[i]][["oflResults"]][[nm]])) {v[i,nm]<-mcmc[[i]][["oflResults"]][[nm]];}
    }
  }
  dfr<-data.frame(v);
  dfr$avgRec<-dfr$avgRecM+dfr$avgRecF;
  nms<-c("objFun","OFL","Fofl","prjB","curB","Fmsy","Bmsy","MSY","B100","avgRec");
  dfr<-dfr[!is.na(dfr$OFL),nms];#drop rows with possibly-NA results
  return(dfr);
}
