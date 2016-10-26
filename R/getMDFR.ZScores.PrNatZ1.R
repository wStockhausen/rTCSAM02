#'
#'@title Get residuals from size frequency fits as dataframe
#'
#' @description  Get Pearson's residuals and negative log-likelihood components
#' from size frequency fits as dataframe.
#'
#' @param fits - list of fits
#' @param mc - model configuration list
#' @param type - type of residuals ('pearsons' or 'nlls')
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
#' @export
#'
getMDFR.ZScores.PrNatZ1<-function(fits,
                                  mc,
                                  type=c("pearsons",'nlls'),
                                  verbose=FALSE){
    if (verbose) cat("---Starting rTCSAM02::getMDFR.ZScores.PrNatZ1(...)\n");
    options(stringsAsFactors=FALSE);

    type<-type[1];

    dims<-mc$dims;
    sxs<-gsub("_"," ",tolower(c(dims$x$nms,"ALL_SEX")),     fixed=TRUE);
    mss<-gsub("_"," ",tolower(c(dims$m$nms,"ALL_MATURITY")),fixed=TRUE);
    scs<-gsub("_"," ",tolower(c(dims$s$nms,"ALL_SHELL")),   fixed=TRUE);
    zbs<-dims$z$vls;

    n<-length(fits);
    yrs<-min(as.numeric(names(fits)),na.rm=TRUE):max(as.numeric(names(fits)),na.rm=TRUE)

    dms<-c(length(sxs),length(mss),length(scs),length(yrs),length(zbs));
    dmnames<-list(x=sxs,m=mss,s=scs,y=yrs,z=zbs);
    pAtZ<-array(0,dms,dmnames);#pearson's residuals
    nAtZ<-array(0,dms,dmnames);#nll's residuals

    yrsp<-names(fits);
    for (i in 1:(n-1)){
        fit<-fits[[i]];
        x<-gsub("_"," ",tolower(fit$x),fixed=TRUE);
        m<-gsub("_"," ",tolower(fit$m),fixed=TRUE);
        s<-gsub("_"," ",tolower(fit$s),fixed=TRUE);
        y<-yrsp[i];
        pAtZ[x,m,s,y,]<-fit$fit$zscrs;
        nAtZ[x,m,s,y,]<-fit$fit$nlls;
    }

    pdfr<-reshape2::melt(pAtZ,value.name='val');
    ndfr<-reshape2::melt(nAtZ,value.name='val');

    #remove all-zero factor combinations
    for (x in sxs){
        for (m in mss){
            for (s in scs){
                pAtZp<-t(as.matrix(pAtZ[x,m,s,,]));
                if (sum(abs(pAtZp),na.rm=TRUE)==0){
                    idx<-(pdfr$x==x)&(pdfr$m==m)&(pdfr$s==s);
                    pdfr<-pdfr[!(idx),];
                    idx<-(ndfr$x==x)&(ndfr$m==m)&(ndfr$s==s);
                    ndfr<-ndfr[!idx,];
                } else {
                    if (verbose) cat('----keeping factor combination',x,m,s,"\n")
                }
            }#s
        }#m
    }#x

    pdfr$type<-'pearsons';
    ndfr$type<-'nlls';

    if (type=='pearsons') mdfr<-pdfr;
    if (type=='nlls')     mdfr<-ndfr;

    if (verbose) cat("---Finished rTCSAM02::getMDFR.ZScoresForSizeComps(...)\n");
    return(mdfr);
}
