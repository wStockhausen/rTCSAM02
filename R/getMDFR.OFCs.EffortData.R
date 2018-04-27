#'
#' @title Get objective function components for effort data from a tcsam02.rep object
#'
#' @description Function to get objective function components for effort data from a tcsam02.rep object.
#'
#' @param obj - a tcsam02.resLst or tcsam02.rep object
#' @param verbose - flag (T/F) to print diagnostic info
#'
#' @return a dataframe
#'
#' @details Returned dataframe has columns:
#' \itemize{
#'   \item{case - model case (blank, to be filled in by caller)}
#'   \item{category - "effort data"}
#'   \item{fleet - dummy value ("")}
#'   \item{catch.type - dummy value ("")}
#'   \item{data.type - data type ("effort data")}
#'   \item{fit.type - dummy value ("")}
#'   \item{nll.type - likelihood type}
#'   \item{y - year (dummy = 'all)}
#'   \item{x - sex}
#'   \item{m - maturity state ("immature")}
#'   \item{s  - shell condition ("new shell")}
#'   \item{ rmse - root mean squared error}
#'   \item{ wgt - likelihood weight}
#'   \item{nll - (unweighted) negative log-likelihood}
#'   \item{objfun - objective function value}
#' }
#'
#' @export
#'
getMDFR.OFCs.EffortData<-function(obj,
                                 verbose=FALSE){

    category<-"effortdata";
    if (inherits(obj,"tcsam02.resLst")){
        #pull out tcsam02.rep object and process
        mdfr<-getMDFR.OFCs.EffortData(obj$rep,verbose);
        return(mdfr);
    } else if (!inherits(obj,"tcsam02.rep")){
        cat("--!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!--\n")
        cat("Error in rTCSAM02::getMDFR.OFCs.EffortData().\n")
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
        arr<-getObj("nlls",dc);
        mdfr<-reshape2::melt(arr,value.name='val',as.is=TRUE);
        mdfr<-mdfr[mdfr$val!=0.0,];
        if (verbose) {
            cat("fleet:",dc$f,"\n")
            cat("datatype:",dcnm,"\n")
            cat("wgt:",dc$wgt,"\n")
            cat("dfr(array):",names(mdfr),"\n")
            cat("x=",mdfr$x,"\n")
            cat("m=",mdfr$m,"\n")
            cat("s=",mdfr$s,"\n")
            cat("nlls=",mdfr$val,"\n")
        }
        rw<-data.frame(case="",
                       category="effort data",
                       fleet=dc$f,
                       catch.type="fishery",
                       data.type=dcnm,
                       fit.type="",
                       nll.type="lognormal",
                       y='all',x=mdfr$x,m=mdfr$m,s=mdfr$s,
                       rmse=dc$stdv,
                       wgt=dc$wgt,
                       nll=mdfr$val,
                       objfun=dc$wgt*mdfr$val,
                       stringsAsFactors=FALSE);
        dfr<-rbind(dfr,rw);
    }#dcnm

    return(dfr);
}
