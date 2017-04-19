#'
#'@title Get model fits to OFL results
#'
#'@description Function to get model fits to OFL results.
#'
#'@param objs - single tcsam02 resLst object, or named list of them
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return dataframe
#'
#'@details Returned dataframe is NOT in canonical format
#'
#'@export
#'
getMDFR.OFLResults<-function(objs,
                             verbose=FALSE){
    if (verbose) cat("--Starting rTCSAM02::getMDFR.OFLResults().\n");
    options(stringsAsFactors=FALSE);

    mdfr<-NULL;
    if ((class(objs)[1]=='list')&&inherits(obj[[1]],"tcsam02.resLst")){
        #objs should be a list of tcsam02 resLst objects
        for (nm in names(objs)){
            mdfrp<-getMDFR.OFLResults(objs[[nm]],
                                          verbose=verbose);
            if (!is.null(mdfrp)) mdfrp$case<-nm;
            mdfr<-rbind(mdfr,mdfrp);
        }
    } else if (inherits(objs,'tcsam02.resLst')){
        #objs is a single tcsam02 resLst object
        ofl<-objs$rep$oflResults;
        if (!is.null(ofl)){
            idx<-!(names(ofl) %in% c("eqNatZF0_xmsz","eqNatZFM_xmsz"))
            mdfr<-as.data.frame(list(case="",ofl[names(ofl)[idx]]));
        }
    }

    if (verbose) cat("--Finished rTCSAM02::getMDFR.OFLResults().\n");
    return(mdfr);
}
