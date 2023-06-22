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
#'  \item{type - variable type}
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
    zbsm<-dims$z$vls;#--model size bins

    n<-length(fits);
    yrsp<-names(fits);
    plst = list();
    nlst = list();
    for (i in 1:(n-1)){
        #--testing: i=1;
        fit<-fits[[i]];
        #--define single-year arrays
        zbs<-fit$zBs;
        if (is.null(zbs)) zbs<-zbsm;
        x<-gsub("_"," ",tolower(fit$x),fixed=TRUE);
        m<-gsub("_"," ",tolower(fit$m),fixed=TRUE);
        s<-gsub("_"," ",tolower(fit$s),fixed=TRUE);
        pdfr<-tibble::tibble(x=x,m=m,s=s,y=yrsp[i],z=zbs,val=fit$fit$zscrs);
        ndfr<-tibble::tibble(x=x,m=m,s=s,y=yrsp[i],z=zbs,val=fit$fit$nlls);
        plst[[i]] = pdfr;
        nlst[[i]] = ndfr;
    }
    pdfr = dplyr::bind_rows(plst); rm(plst);
    ndfr = dplyr::bind_rows(nlst); rm(nlst);


    pdfr$type<-'pearsons';
    ndfr$type<-'nlls';

    if (type=='pearsons') mdfr<-pdfr;
    if (type=='nlls')     mdfr<-ndfr;

    if (verbose) cat("---Finished rTCSAM02::getMDFR.ZScores.PrNatZ1(...)\n");
    return(mdfr);
}
