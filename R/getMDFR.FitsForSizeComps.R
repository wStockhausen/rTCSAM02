#'
#' @title Get fits for size comps as a dataframe
#'
#' @description  Get fits for size comps as a dataframe.
#'
#' @param fits - list of fits for size compositions
#' @param mc - model configuration list
#' @param verbose - flag (T/F) to print diagnostic info
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
#' @import dplyr
#'
#' @export
#'
getMDFR.FitsForSizeComps<-function(fits,
                                   mc,
                                   verbose=FALSE){
    if (verbose) cat("---Starting rTCSAM02::getMDFR.FitsForSizeComps(...)\n");
    options(stringsAsFactors=FALSE);

    n<-length(fits);
    yrsp<-names(fits);#--years with observed size comps

    lst = list();
    for (i in 1:(n-1)){
        #--testing: i=1;
        fit<-fits[[i]];
        #--define single-year arrays
        zbs<-fit$zBs;
        x<-gsub("_"," ",tolower(fit$x),fixed=TRUE);
        m<-gsub("_"," ",tolower(fit$m),fixed=TRUE);
        s<-gsub("_"," ",tolower(fit$s),fixed=TRUE);
        mdfr<-tibble::tibble(x=x,m=m,s=s,y=yrsp[i],z=zbs,var="predicted",val=fit$fit$mod);
        odfr<-tibble::tibble(x=x,m=m,s=s,y=yrsp[i],z=zbs,var="observed",val=fit$fit$obs);
        lst[[i]] = dplyr::bind_rows(odfr,mdfr);
        rm(fit,zbs,x,m,s,odfr,mdfr);
    }
    mdfr = dplyr::bind_rows(lst); rm(lst);

    if (verbose) cat("---Finished rTCSAM02::getMDFR.FitsForSizeComps(...)\n");
    return(mdfr)
}
