#'
#' @title Extract all variables from an MCMC results list
#'
#' @description Function to extract all variables from an MCMC results list.
#'
#' @param mcmc - an MCMC results list from a TCSAM02 model
#'
#' @return a list, with elements named for each variable
#'
#' @details Calls mcmc.ExtractVar to extract individual variables.
#'
#' @export
#'
mcmc.ExtractVars<-function(mcmc){
    vrs<-names(mcmc[[1]]);
    res<-list();
    for (vr in vrs){
        res[[vr]]<-mcmc.ExtractVar(mcmc,vr);
    }
    return(res);
}

#'
#' @title Extract a variable from an MCMC results list
#'
#' @description Function to extract a variable from an MCMC results list.
#'
#' @param mcmc - an MCMC results list from a TCSAM02 model
#' @param vr - variable name to extract
#'
#' @return a vector or an array, depending on the variable
#'
#' @details handles numeric and array mcmc variables. If the variable is a
#' n-dimensional array, the returned array will be (n+1)-dimensional, with the
#' mcmc iteration as the left-most dimension.
#'
#' @export
#'
mcmc.ExtractVar<-function(mcmc,vr){
    nrws<-length(mcmc)-1;
    tmp<-mcmc[[1]][[vr]];
    if (class(tmp)[1]=="numeric") {
        rz = vector("numeric",length=nrws);
        for (rw in 1:nrws) rz[rw]<-mcmc[[rw]][[vr]];
        return(rz);
    }
    if (class(tmp)[1]=="array") {
        dms<-dim(tmp);
        dmnms<-dimnames(tmp);
        rz = array(dim=c(nrws,dms),dimnames=dmnms);
        for (rw in 1:nrws) rz[rw]<-mcmc[[rw]][[vr]];
        return(rz);
    }
    return(NULL);
}
