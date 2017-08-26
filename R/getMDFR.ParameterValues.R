#'
#' @title Get parameter values from a set of TCSAM02 model runs as a dataframe
#'
#' @description Function to get parameter values from a set of TCSAM02 model runs as a dataframe.
#'
#' @param tcsams - a tcsam02.resLst object or named list of such
#' @param verbose - flag to print debugging info
#'
#' @return - a dataframe
#'
#' @details Returns a dataframe with parameter estimates and standard deviations (if the std object exists) by model run.
#'
#' @export
#'
getMDFR.ParameterValues<-function(tcsams,verbose=FALSE){
    if (verbose) cat("--starting rTCSAM02::getMDFR.ParameterValues().\n");
    options(stringsAsFactors=FALSE);

    if (inherits(tcsams,'tcsam02.resLst')){
        #tcsams is a tcsam02 resLst object
        #do nothing and drop out of "if" statement
    } else if (inherits(tcsams,'list')){
        #tcsams is a list of tcsam02.resLst objects
        mdfr<-NULL;
        nl<-length(tcsams);
        nms<-names(tcsams);
        for (l in 1:nl){
            if (verobse) cat("Processing",nms[l],"\n")
            tcsam1<-tcsams[[l]];
            mdfrp<-getMDFR.ParameterValues(tcsams=tcsam1,verbose=verbose);
            if (!is.null(mdfrp)){
                if (!is.null(nms[l])) mdfrp$case<-nms[l];
                mdfr<-rbind(mdfr,mdfrp);
            }
        }
        return(mdfr);
    } else {
        cat("Error in getMDFR.ParameterValues(tcsams).\n")
        cat("'tcsams' should be a 'tcsam02.resLst' object or a list of such.\n")
        cat("Returning NULL.\n")
        return(NULL);
    }

    #tcsams is a single tcsam02.resLst object
    if (verbose) cat("\nProcessing resLst object")
    #want to combine prs and std objects
    mdfrp<-tcsams$prs;
    std<-tcsams$std;
    mdfrp$name<-gsub(" ","",mdfrp$name,fixed=TRUE);
    mdfrp$stdv<-NA;
    if (!is.null(std)){
        uPNs<-unique(mdfrp$name);
        for (uPN in uPNs){
            idp<-which(mdfrp$name==uPN);
            ids<-which(std$name==uPN);
            if (length(ids)>0) {
                mdfrp$stdv[idp]<-std$std.dev[ids];
            }
        }
    }

    mdfr<-mdfrp;
    return(mdfr);
}
