extractMCMC.Vars<-function(mcmc){
    vrs<-names(mcmc[[1]]);
    res<-list();
    for (vr in vrs){
        res[[vr]]<-extractMCMC.Var(mcmc,vr);
    }
    return(res);
}

#'
#' @title Extract a variable from an MCMC results list
#'
#' @description Function to extract a variable from an MCMC results list
#'
#' @param mcmc - an MCMC results list from a TCSAM02 model
#' @param vr - variable name to extract
#'
#' @result
extractMCMC.Var<-function(mcmc,vr){
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
