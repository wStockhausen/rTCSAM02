#'
#'@title Get model arrays as a melted dataframe from TCSAM02 model runs
#'
#'@description Function to get model objects as a melted dataframe from TCSAM2015 model runs.
#'
#'@param path - path in models to requested array (using '/' as separator for list levels)
#'@param tcsams - single tcsam02.resLst object, or named list of such
#'@param verbose - flag (T/F) to print diagnostics
#'
#'@return Melted dataframe (ala package reshape2).
#'
#'@details Returned dataframe is a melted (ala reshape2) version of the requested array,
#'with additional columns 'model' and 'modeltype' appended at the "right". The array value
#'is in column 'val'. Uses \code{reshape2::melt(...)}.
#'
#'@export
#'
getMDFR<-function(path,tcsams,verbose=FALSE){
    mdfr<-NULL;
    if (inherits(tcsams,'tcsam02.rep')){
        #tcsams is a tcsam02 report object
        obj<-getObj(path,tcsams,verbose=verbose);
        if (!is.null(obj)){
            mdfr<-reshape2::melt(obj,value.name='val',as.is=TRUE);
            mdfr$case<-'tcsam';
        }
    } else if (inherits(tcsams,'tcsam02.resLst')){
        #tcsams is a tcsam02 resLst object
        obj<-getObj(path,tcsams$rep,verbose=verbose);
        if (!is.null(obj)){
            mdfr<-reshape2::melt(obj,value.name='val',as.is=TRUE);
            mdfr$case<-'tcsam';
        }
    } else if (inherits(tcsams,'list')){
        #tcsams is a list of tcsam02.resLst objects
        nl<-length(tcsams);
        nms<-names(tcsams);
        for (l in 1:nl){
            tcsam1<-tcsams[[l]];
            mdfrp<-getMDFR(path,tcsams=tcsam1,verbose=verbose);
            if (!is.null(mdfrp)){
                if (!is.null(nms[l])) mdfrp$case<-nms[l];
                mdfr<-rbind(mdfr,mdfrp);
            }
        }
    } else {
        cat("Error in getMDFR(path,tcsams).\n")
        cat("'tcsams' should be a 'tcsam02.rep' object, a 'tcsam02.resLst' object, or a list of the latter.\n")
        cat("Returning NULL.\n")
        return(NULL);
    }

    # if (!is.null(mdfr)){
    #     cns<-colnames(mdfr);
    #     chks<-c('y','z','zp');
    #     for (chk in chks){
    #         idx<-which(cns==chk);
    #         if (length(idx)>0) mdfr[,chk]<-as.numeric(mdfr[,chk]);
    #     }
    # }

    if (!is.null(mdfr)) mdfr<-rCompTCMs::getMDFR.CanonicalFormat(mdfr);
    return(mdfr);
}
