#'
#' @title Get objective function components for growth data from a tcsam02.rep object
#'
#' @description Function to get objective function components for growth data from a tcsam02.rep object.
#'
#' @param obj - a tcsam02.resLst or tcsam02.rep object
#' @param verbose - flag (T/F) to print diagnostic info
#'
#' @return a dataframe
#'
#' @details Returned dataframe has columns:
#' \itemize{
#'   \item{case - model case (blank, to be filled in by caller)}
#'   \item{category - "growth data"}
#'   \item{fleet - dummy value ("")}
#'   \item{catch.type - dummy value ("")}
#'   \item{data.type - data type ("growth data")}
#'   \item{fit.type - dummy value ("")}
#'   \item{nll.type - likelihood type}
#'   \item{y - year (dummy = 'all)}
#'   \item{x - sex}
#'   \item{m - maturity state ("immature")}
#'   \item{s  - shell condition ("new shell")}
#'   \item{ wgt - likelihood weight}
#'   \item{nll - (unweighted) negative log-likelihood}
#'   \item{objfun - objective function value}
#' }
#'
getMDFR.OFCs.GrowthData<-function(obj,
                                 verbose=FALSE){

    category<-"growthdata";
    if (inherits(obj,"tcsam02.rep")){
        #do nothing, will fall out to code below
    } else if (inherits(obj,"tcsam02.resLst")){
        #pull out tcsam02.rep object and process
        mdfr<-getMDFR.OFCs.GrowthData(obj$rep,verbose);
        return(mdfr);
    } else {
        cat("--!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!--\n")
        cat("Error in rTCSAM02::getMDFR.OFCs.GrowthData().\n")
        cat("Input object not reducible to a tcsam02.rep object!\n")
        cat("Classes = ",class(obj),"\n");
        cat("Returning NULL.\n")
        cat("--!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!--\n")
        return(NULL);
    }

    #obj should now be a tcsam02.rep object
    lst<-obj$model.fits[[category]];

    dfr<-NULL;
    dcnms<-names(lst);#data components
    dcnms<-dcnms[dcnms!=''];
    for (dcnm in dcnms){
        if (verbose) cat("Processing data component",dcnm,"\n")
        dc<-lst[[dcnm]];
        xnms<-names(dc);
        xnms<-xnms[xnms!=''];
        for (xnm in xnms){
            fit<-dc[[xnm]];
            if (!is.null(fit)){
                rw<-data.frame(case="",
                               category="growth data",
                               fleet="",
                               catch.type="",
                               data.type=dcnm,
                               fit.type="",
                               nll.type=fit$type,
                               y='all',x=tolower(xnm),m="immature",s="new shell",
                               wgt=fit$wgt,nll=fit$nll,objfun=fit$objfun);
                dfr<-rbind(dfr,rw);
            }
        }#xnm
    }#dcnm

    return(dfr);
}
