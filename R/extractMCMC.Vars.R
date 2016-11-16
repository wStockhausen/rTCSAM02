extractMCMC.Vars<-function(mcmc){
    vrs<-names(mcmc[[1]]);
    res<-list();
    for (vr in vrs){
        res[[vr]]<-extractMCMC.Var(mcmc,vr);
    }
    return(res);
}

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
        rz = array(dim=c(nrws,dms),dimnames=);
        for (rw in 1:nrws) rz[rw]<-mcmc[[rw]][[vr]];
        return(rz);
    }
    return(NULL);
}
