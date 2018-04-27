#'
#' @title Get data-related components from the objective function for a (list of) TCSAM02 model(s) as a dataframe
#'
#' @description Function to get data-related components from the objective function for a (list of) TCSAM02 model(s) as a dataframe.
#'
#' @param obj - a single tcsam02 rep object, a single tcsam02 resLst object, or a named list of such
#' @param categories - data-related objective function components to get ("all","surveys","fisheries","growthdata","maturitydata","effortdata")
#' @param verbose - flag (T/F) to print diagnostic info
#'
#' @return dataframe
#'
#' @details Returned dataframe has columns:
#' \itemize{
#'   \item{case - model case (blank, to be filled in by caller if obj is not a list of tcsam02.resLst objects)}
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
#'   \item{ rmse - root mean squared error}
#'   \item{wgt - likelihood weight}
#'   \item{nll - (unweighted) negative log-likelihood}
#'   \item{objfun - objective function value}
#' }
#'
#' @export
#'
getMDFR.OFCs.DataComponents<-function(obj,
                                      categories=c("all","surveys","fisheries","growthdata","maturitydata","effortdata"),
                                      verbose=FALSE){
    if (verbose) cat("Starting rTCSAM02::getMDFR.OFCs.DataComponents().\n")
    options(stringsAsFactors=FALSE);
    mdfr<-NULL;
    if ((class(obj)[1]=="list") ){
        if (verbose) cat("--Processing list\n",sep='')
        for (case in names(obj)){
            if (verbose) cat("\tProcessing list element '",case,"'\n",sep='')
            dfr<-getMDFR.OFCs.DataComponents(obj[[case]],categories,verbose);
            if (!is.null(dfr)){
                dfr$case<-case;
                mdfr<-rbind(mdfr,dfr);
            }
        }
        return(mdfr);
    } else if (inherits(obj,"tcsam02.resLst")){
        #pull out tcsam02.rep object and process
        if (verbose) cat("--Processing tcsam02.resLst object\n",sep='')
        mdfr<-getMDFR.OFCs.DataComponents(obj$rep,categories,verbose);
        return(mdfr);
    } else  if (!inherits(obj,"tcsam02.rep")){
        cat("--!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!--\n")
        cat("Error in rTCSAM02::getMDFR.OFCs.DataComponents().\n")
        cat("Input object not reducible to a tcsam02.rep object!\n")
        cat("Classes = ",class(obj),"\n");
        cat("Returning NULL.\n")
        cat("--!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!--\n")
        return(NULL);
    }

    #obj should now be a tcsam02.rep object
    if (verbose) cat("----Processing tcsam02.rep object\n",sep='')

    if ("all" %in% categories) categories<-c("surveys","fisheries","growthdata","maturitydata","effortdata");

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
    if ("maturitydata" %in% categories){
        dfr<-getMDFR.OFCs.MaturityData(obj,verbose=verbose);
        mdfr<-rbind(mdfr,dfr);
    }
    if ("effortdata" %in% categories){
        dfr<-getMDFR.OFCs.EffortData(obj,verbose=verbose);
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
