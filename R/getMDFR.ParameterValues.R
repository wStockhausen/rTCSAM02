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
            if (verbose) cat("Processing",nms[l],"\n")
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
    if (verbose) cat("\nProcessing resLst object\n")
    #want to combine prs and std objects
    mdfrp<-tcsams$prs;
    std<-tcsams$std;
    mdfrp$name<-gsub(" ","",mdfrp$name,fixed=TRUE);
    mdfrp$stdv<-NA;
    if (!is.null(std)){
        npar<-nrow(mdfrp);       #--number of parameters
        nact<-sum(mdfrp$phase>0);#--number of active parameters
        s<-0;
        for (p in 1:npar){
          if (mdfrp$phase[p]>0){
            s<-s+1; mdfrp[p,"stdv"]<-std[s,"std.dev"];
          }
        }
        if (s!=nact) {
            cat("Error in getMDFR.ParameterValues(tcsams).\n")
            cat("Did not match up std file with active parameters!")
            cat("Returning NULL.\n")
            return(NULL);
        }
    }

    #cat(names(mdfrp))
    mdfrp$label<-gsub("_"," ",mdfrp$label,fixed=TRUE);

    mdfr<-cbind(case="tcsam",mdfrp);
    return(mdfr);
}
