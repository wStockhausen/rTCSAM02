#'
#' @title Get effective Ns for size comps as a dataframe
#'
#' @description  Get get effective Ns for size comps as a dataframe.
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
#'  \item{val - value}
#'  \item{var - variable type ('input ss' or 'effective N')}
#' }
#'
#' @export
#'
getMDFR.EffectiveNsForSizeComps<-function(fits,
                                           mc,
                                           verbose=FALSE){
    if (verbose) cat("---Starting rTCSAM02::getMDFR.EffectiveNsForSizeComps(...)\n");
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

    dms<-c(length(sxs),length(mss),length(scs),length(yrs),2);
    dmnames<-list(x=sxs,m=mss,s=scs,y=yrs,var=c("input ss","effective N"));
    effNs<-array(0,dms,dmnames);#effective N's and input sample sizes

    dms<-c(length(sxs),length(mss),length(scs),length(yrs));
    dmnames<-list(x=sxs,m=mss,s=scs,y=yrs);

    for (i in 1:(n-1)){
        fit<-fits[[i]];
        x<-gsub("_"," ",tolower(fit$x),fixed=TRUE);
        m<-gsub("_"," ",tolower(fit$m),fixed=TRUE);
        s<-gsub("_"," ",tolower(fit$s),fixed=TRUE);
        y<-yrsp[i];
        effNs[x,m,s,y,"input ss"]   <-fit$fit$ss;
        effNs[x,m,s,y,"effective N"]<-fit$fit$effN;
    }

    dfr<-reshape2::melt(effNs,value.name='val');

    dfr<-dfr[dfr$y %in% yrsp,];#select only years with observed size comps
    dfr<-dfr[dfr$val>0,];#select only rows with sample sizes > 0
    # mdfr<-NULL;
    # for (x in sxs){
    #     for (m in mss){
    #         for (s in scs){
    #             #set up extraction indices
    #             idx<-(dfr$x %in% x)&(dfr$m %in% m)&(dfr$s %in% s);
    #             #??if (sum(pdfr$val[idx&odx],na.rm=TRUE)>0){
    #                 if (verbose) cat('----keeping factor combination',x,m,s,"\n")
    #                 mdfr<-rbind(mdfr,pdfr[idx,]);
    #             #}#sum(pdfr$val[idx&odx],na.rm=TRUE)>0
    #         }#s
    #     }#m
    # }#x

    if (verbose) cat("---Finished rTCSAM02::getMDFR.EffectiveNsForSizeComps(...)\n");
    return(dfr)
}
