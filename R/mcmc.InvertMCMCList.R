#'
#' @title Invert an mcmc list
#'
#' @description Function to invert an mcmc list
#'
#' @param mcmc - mcmc list
#' @param verbose - T/F to print diagnostic info
#'
#' @return a dataframe
#'
#' @details None
#'
#' @export
#'
mcmc.InvertMCMCList<-function(mcmc,verbose=FALSE){
  nms<-names(mcmc[[1]]);
  lst<-list();
  for (nm in nms){
    if (verbose) cat("checking",nm,"\n")
    xlst<-mcmc.InvertListElement(mcmc,nm);
    if (!is.null(xlst)) {
      if (verbose) cat("Adding",nm,"to list.\n")
      for (xnm in names(xlst)){
        nmp<-paste0(nm,"[",xnm,"]");
        lst[[nmp]]<-xlst[[xnm]];
      }
    }
  }
  dfr<-data.frame(lst,check.names=FALSE);
  return(dfr);
}


