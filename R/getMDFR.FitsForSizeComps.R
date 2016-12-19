#'
#' @title Get fits for size comps as a dataframe
#'
#' @description  Get fits for size comps as a dataframe.
#'
#' @param fits - list of fits for size compositions
#' @param mc - model configuration list
#' @param verbose - flag (T/F) to print dagnostic info
#'
#' @return dataframe
#'
#' @details Uses \code{reshape2::melt(...)}.
#' Returned dataframe has columns:
#' \itemize{
#'  \item{x - sex}
#'  \item{m - maturity}
#'  \item{s - shell condition}
#'  \item{y - year}
#'  \item{z - size bin}
#'  \item{val - value}
#'  \item{var - variable type}
#' }
#'
getMDFR.FitsForSizeComps<-function(fits,
                                   mc,
                                   verbose=FALSE){
    if (verbose) cat("---Starting rTCSAM02::getMDFR.FitsForSizeComps(...)\n");
    options(stringsAsFactors=FALSE);

    dims<-mc$dims;
    sxs<-gsub("_"," ",tolower(c(dims$x$nms,"ALL_SEX")),     fixed=TRUE);
    mss<-gsub("_"," ",tolower(c(dims$m$nms,"ALL_MATURITY")),fixed=TRUE);
    scs<-gsub("_"," ",tolower(c(dims$s$nms,"ALL_SHELL")),   fixed=TRUE);
    zbs<-dims$z$vls;

    n<-length(fits);
    yrsp<-names(fits);
    yrs<-min(as.numeric(names(fits)),na.rm=TRUE):max(as.numeric(names(fits)),na.rm=TRUE)
    #yrs<-sort(as.numeric(yrsp));

    dms<-c(length(sxs),length(mss),length(scs),length(yrs),length(zbs));
    dmnames<-list(x=sxs,m=mss,s=scs,y=yrs,z=zbs);
    oAtZ<-array(0,dms,dmnames);#observed size comps
    mAtZ<-array(0,dms,dmnames);#model size comps

    dms<-c(length(sxs),length(mss),length(scs),length(yrs));
    dmnames<-list(x=sxs,m=mss,s=scs,y=yrs);

    for (i in 1:(n-1)){
        fit<-fits[[i]];
        x<-gsub("_"," ",tolower(fit$x),fixed=TRUE);
        m<-gsub("_"," ",tolower(fit$m),fixed=TRUE);
        s<-gsub("_"," ",tolower(fit$s),fixed=TRUE);
        y<-yrsp[i];
        oAtZ[x,m,s,y,]<-fit$fit$obs;
        mAtZ[x,m,s,y,]<-fit$fit$mod;
    }

    odfr<-reshape2::melt(oAtZ,value.name='val');
    mdfr<-reshape2::melt(mAtZ,value.name='val');
    odfr$var<-'observed';
    mdfr$var<-'predicted';
    pdfr<-rbind(odfr,mdfr);

    pdfr<-pdfr[pdfr$y %in% yrsp,];#select only years with observed size comps

    rm(odfr,mdfr);

    mdfr<-NULL;
    odx<-(pdfr$var=='observed');
    for (x in sxs){
        for (m in mss){
            for (s in scs){
                #set up extraction indices
                idx<-(pdfr$x %in% x)&(pdfr$m %in% m)&(pdfr$s %in% s);
                if (sum(pdfr$val[idx&odx],na.rm=TRUE)>0){
                    if (verbose) cat('----keeping factor combination',x,m,s,"\n")
                    mdfr<-rbind(mdfr,pdfr[idx,]);
                }#sum(pdfr$val[idx&odx],na.rm=TRUE)>0
            }#s
        }#m
    }#x

    if (verbose) cat("---Finished rTCSAM02::getMDFR.FitsForSizeComps(...)\n");
    return(mdfr)
}
