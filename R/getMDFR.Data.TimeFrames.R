#'
#'@title Get time frames for model data components as a dataframe
#'
#'@description Function to get time frames for model data components as a dataframe.
#'
#'@param objs - single model resLst object, or named list of them
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return dataframe
#'
#'@details TODO: COMPLETE THIS!!
#'Returned dataframe has the following columns:
#'
#'@export
#'
getMDFR.Data.TimeFrames<-function(objs,
                                 verbose=FALSE){
    options(stringsAsFactors=FALSE);

    mdfr<-NULL;
    if (class(objs)[1]=='list'){
        #repObjs should be a list of tcsam02 resLst objects
        for (nm in names(objs)){
            mdfrp<-getMDFR.Data.TimeFrames(objs[[nm]],
                                          verbose=verbose);
            if (!is.null(mdfrp)) mdfrp$case<-nm;
            mdfr<-rbind(mdfr,mdfrp);
        }
    } else if (inherits(objs,'tcsam02.resLst')){
        if (verbose) cat("--Starting rTCSAM02::getMDFR.Data.TimeFrames().\n");
        #objs is a single tcsam02 resLst object
    }


    if (verbose) cat("--Finished getMDFR.Data.TimeFrames().\n");
    return(mdfr);
}
