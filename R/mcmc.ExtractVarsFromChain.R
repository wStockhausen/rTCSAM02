#'
#' @title Extract all variables from an MCMC results list for a single chain
#'
#' @description Function to extract all variables from an MCMC results list for a single chain.
#'
#' @param mcmc - an MCMC results list for a single chain from a TCSAM02 model
#' @param verbose - flag to print diagnostic info
#'
#' @return a list, with elements named for each variable
#'
#' @details Calls [mcmc.ExtractVarFromChain()] to extract individual variables.
#'
#' @export
#'
mcmc.ExtractVarsFromChain<-function(mcmc,verbose=FALSE){
  vrs = names(mcmc[[1]]);
  lst = list();
  for (vr in vrs){
    if (verbose) cat("Processing",vr,"\n");
    lst[[vr]] = mcmc.ExtractVarFromChain(mcmc,vr);
  }
  return(lst);
}

#'
#' @title Extract a variable from an MCMC results list for a single chain
#'
#' @description Function to extract a variable from an MCMC results list for a single chain.
#'
#' @param mcmc - an MCMC results list for a single chain from a TCSAM02 model
#' @param vr - variable name to extract
#' @param verbose - flag to print diagnostic info
#'
#' @return a list, vector, or array, depending on the variable
#'
#' @details handles lists of numeric and array mcmc variables. If the variable is a
#' n-dimensional array, the returned array will be (n+1)-dimensional, with the
#' mcmc iteration as the left-most dimension.
#'
#' @export
#'
mcmc.ExtractVarFromChain<-function(mcmc,vr,verbose=FALSE){
    lst = list();
    nrws<-length(mcmc)-1;
    tmp<-mcmc[[1]][[vr]];
    if (is.null(tmp)) return(NULL);
    if (class(tmp)[1]=="list"){
      nms = names(tmp);
      for (nm in nms){
        if (verbose) cat("\tProcessing",nm,"\n")
        var = paste0(vr,"[",nm,"]");
        if (class(tmp[[nm]])[1]=="numeric") {
          rz = vector("numeric",length=nrws);
          for (rw in 1:nrws) rz[rw]<-mcmc[[rw]][[vr]][[nm]];
        }
        if (class(tmp)[1]=="array") {
          dms<-dim(tmp);
          dmnms<-dimnames(tmp);
          if (is.null(names(dmnms))) names(dmnms) = paste0("dim_",1:length(dmnms));
          dimnames=list(rows=1:nrws);
          for (dmnm in names(dmnms)) {dimnames[[dmnm]] = dmnms[[dmnm]];}
          rz = array(dim=c(nrws,dms),dimnames=dimnames);
          for (rw in 1:nrws) rz[rw,]<-mcmc[[rw]][[vr]];
        }
        lst[[var]] = rz;
      }#--nms loop
      return(lst);
    }
    if (class(tmp)[1]=="numeric") {
      rz = vector("numeric",length=nrws);
      for (rw in 1:nrws) rz[rw]<-mcmc[[rw]][[vr]];
      lst[[vr]] = rz;
      return(lst);
    }
    if (class(tmp)[1]=="array") {
      dms<-dim(tmp);
      dmnms<-dimnames(tmp);
      if (is.null(names(dmnms))) names(dmnms) = paste0("dim_",1:length(dmnms));
      dimnames=list(rows=1:nrws);
      for (dmnm in names(dmnms)) {dimnames[[dmnm]] = dmnms[[dmnm]];}
      rz = array(dim=c(nrws,dms),dimnames=dimnames);
      for (rw in 1:nrws) rz[rw,]<-mcmc[[rw]][[vr]];
      lst[[vr]] = rz;
      return(lst);
    }
    return(lst);#--empty list
}
