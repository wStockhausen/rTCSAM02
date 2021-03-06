#'
#' @title Get objective function components for maturity ogive data from a tcsam02.rep object
#'
#' @description Function to get objective function components for maturity ogive data from a tcsam02.rep object.
#'
#' @param obj - a tcsam02.resLst or tcsam02.rep object
#' @param verbose - flag (T/F) to print diagnostic info
#'
#' @return a dataframe
#'
#' @details Returned dataframe has columns:
#' \itemize{
#'   \item{case - model case (blank, to be filled in by caller)}
#'   \item{category - "maturity ogive data"}
#'   \item{fleet - dummy value ("")}
#'   \item{catch.type - dummy value ("")}
#'   \item{data.type - data type ("maturity ogive data")}
#'   \item{fit.type - dummy value ("")}
#'   \item{nll.type - likelihood type}
#'   \item{y - year}
#'   \item{x - sex}
#'   \item{m - maturity ogive state ("")}
#'   \item{s - shell condition ("new shell")}
#'   \item{ rmse - root mean squared error}
#'   \item{ wgt - likelihood weight}
#'   \item{nll - (unweighted) negative log-likelihood}
#'   \item{objfun - objective function value}
#' }
#'
#' @export
#'
getMDFR.OFCs.MaturityOgiveData<-function(obj,
                                 verbose=FALSE){

    category<-"maturityogivedata";
    if (inherits(obj,"tcsam02.rep")){
        #do nothing, will fall out to code below
    } else if (inherits(obj,"tcsam02.resLst")){
        #pull out tcsam02.rep object and process
        mdfr<-getMDFR.OFCs.MaturityOgiveData(obj$rep,verbose);
        return(mdfr);
    } else {
        cat("--!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!--\n")
        cat("Error in rTCSAM02::getMDFR.OFCs.MaturityOgiveData().\n")
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
            fit<-dc;
            if (!is.null(fit)){
                dfrp<-reshape2::dcast(data=data.frame(y=fit$y,nlls=fit$nlls),
                                      formula=y~.,fun.aggregate=wtsUtilities::Sum,value.var="nlls");
                rw<-data.frame(case="",
                               category="maturity ogive data",
                               fleet=fit$fleet,
                               catch.type="",
                               data.type=dcnm,
                               fit.type="",
                               nll.type=fit$type,
                               y=dfrp$y,x=tolower(fit$sex),m="",s="new shell",
                               rmse=fit$rmse,
                               wgt=fit$wgt,
                               nll=dfrp[["."]],
                               objfun=fit$wgt*dfrp[["."]]);
                dfr<-rbind(dfr,rw);
            }
    }#dcnm

    return(dfr);
}
