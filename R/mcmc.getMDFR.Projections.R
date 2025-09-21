#'
#'@title Convert a single chain MCMC list of projection lists to a [tibble::tibble]
#'
#'@description Function to convert a single chain MCMC list of projection lists to a [tibble::tibble].
#'
#'@param mcmcList - a single chain MCMC list of projection lists parsed using [mcmc.ReadMMCMC(projFile)]
#'
#'@return a [tibble::tibble] representing a single chain MCMC list of projection lists, or NULL. The returned object will be of class 'tcsam02.mcmc.projDFR'.
#'
#'@details The returned object will be a [tibble::tibble] of class 'tcsam02.mcmc.projDFR'.
#'
#'@importFrom dplyr bind_cols
#'@importFrom dplyr bind_rows
#'
#'@export
#'
mcmc.getMDFR.Projections<-function(mcmcList){
  n = length(mcmcList);
  lst = list();
  for (i in 1:n){
    projList = mcmcList[[i]]$projFMsList;
    mdfrp = getMDFR.Projections(projList);
    lst[[i]] = dplyr::bind_cols(mcmc=i,mdfrp);
    rm(mdfrp);
  }
  mdfr = dplyr::bind_rows(lst);
  class(mdfr)<-c('tcsam02.mcmc.projDFR',class(mdfr));
  return(mdfr);
}
