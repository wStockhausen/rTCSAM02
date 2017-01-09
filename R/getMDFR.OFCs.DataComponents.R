#'
#' @title Get data-related components from the objective function for a TCSAM02 model as a dataframe
#'
#' @description Function to get data-related components from the objective function for a TCSAM02 model as a dataframe.
#'
#' @param obj - a tcsam02.resLst or tcsam02.rep object
#' @param categories - data-related objective function components to get ("all","surveys","fisheries","growthdata")
#' @param verbose - flag (T/F) to print diagnostic info
#'
#' @return dataframe
#'
#' @details Returned dataframe has columns:
#' \itemize{
#'   \item{case - model case (blank, to be filled in by caller)}
#'   \item{category - fleet category}
#'   \item{fleet - fleet name}
#'   \item{catch.type - catch type}
#'   \item{data.type - data type}
#'   \item{fit.type - fit type}
#'   \item{nll.type - likelihood type}
#'   \item{y - year}
#'   \item{x - sex}
#'   \item{m - maturity state}
#'   \item{s  - shell condition}
#'   \item{ wgt - likelihood weight}
#'   \item{nll - (unweighted) negative log-likelihood}
#'   \item{objfun - objective function value}
#' }
#'
#' @export
#'
getMDFR.OFCs.DataComponents<-function(obj,
                                      categories=c("all","surveys","fisheries","growthdata"),
                                      verbose=FALSE){
    if (verbose) cat("Starting rTCSAM02::getMDFR.OFCs.DataComponents().\n")
    options(stringsAsFactors=FALSE);
    if (inherits(obj,"tcsam02.rep")){
        #do nothing, will fall out to code below
    } else if (inherits(obj,"tcsam02.resLst")){
        #pull out tcsam02.rep object and process
        mdfr<-getMDFR.OFCs.DataComponents(obj$rep,categories,verbose);
        return(mdfr);
    } else {
        cat("--!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!--\n")
        cat("Error in rTCSAM02::getMDFR.OFCs.DataComponents().\n")
        cat("Input object not reducible to a tcsam02.rep object!\n")
        cat("Classes = ",class(obj),"\n");
        cat("Returning NULL.\n")
        cat("--!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!--\n")
        return(NULL);
    }

    #obj should now be a tcsam02.rep object
    if ("all" %in% categories) categories<-c("surveys","fisheries","growthdata");

    mdfr<-NULL;
    for (dc in c("surveys","fisheries")){
        if (dc %in% categories) {
            dfr<-getMDFR.OFCs.FleetData(obj,category=dc,verbose=verbose);
            mdfr<-rbind(mdfr,dfr);
        }
    }
    if ("growthdata" %in% categories){
        dfr<-getMDFR.OFCs.GrowthData(obj,verbose=verbose);
        mdfr<-rbind(mdfr,dfr);
    }

    mdfr$x<-tolower(mdfr$x);
    mdfr$x<-gsub("all_sex","all sexes",mdfr$x,fixed=TRUE);

    mdfr$m<-tolower(mdfr$m);
    mdfr$m<-gsub("all_maturity","all maturity states",mdfr$m,fixed=TRUE);
    mdfr$m<-gsub("_"," ",mdfr$m,fixed=TRUE);

    mdfr$s<-tolower(mdfr$s);
    mdfr$s<-gsub("all_shell","all shell conditions",mdfr$s,fixed=TRUE);
    mdfr$s<-gsub("_"," ",mdfr$s,fixed=TRUE);

    if (verbose) cat("finished rTCSAM02::getMDFR.OFCs.DataComponents().\n")
    return(mdfr);
}
