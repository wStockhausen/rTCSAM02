#'
#'@title Get cohort progression time series from model results from TCSAM02 model runs as a dataframe
#'
#'@description Function to get cohort progression time series from model results from TCSAM02 model runs as a dataframe.
#'
#'@param tcsams - single tcsam02.rep object, tcsam02.resLst object, or named list of the latter
#'@param cast - casting formula for excluding x,m,s,z factor levels from a sum across the unspecified factors
#'@param path - path into tcsams object at which to find the cohort progression array
#'@param verbose - flag (T/F) to print debug info
#'
#'@return dataframe in canonical format
#'
#'@details Extracts the estimated cohort abundance time series.
#'
#'@export
#'
getMDFR.Pop.CohortProgression<-function(tcsams,
                                        cast="x+m+s+z",
                                        path='cohortprogression/n_yxmsz',
                                        verbose=FALSE){
    if (verbose) cat("--starting rTCSAM02::getMDFR.Pop.CohortProgression().\n");
    options(stringsAsFactors=FALSE);

    if (path=='cohortprogression/n_yxmsz'){
      mdfr<-getMDFR(path,tcsams,verbose);
    } else {
        obj<-getObj(path,tcsams,verbose=verbose);
        if (!is.null(obj)){
            mdfr<-reshape2::melt(obj,value.name='val',as.is=TRUE);
            mdfr$case<-'tcsam';
        }
    }
    mdfr$process<-"population";
    mdfr$fleet<-'';
    mdfr$category<-'';
    mdfr$type<-'predicted';
    mdfr$pc<-';'
    mdfr<-removeImmOS(mdfr);

    castform<-"case+process+fleet+category+type+pc+y";
    if (!is.null(cast)|(cast!='')) castform<-paste0(castform,"+",cast);
    castform<-paste0(castform,"~.");
    if (verbose) cat("casting formula = '",castform,"'\n",sep='')
    ddfr<-reshape2::dcast(mdfr,castform,fun.aggregate=sum,na.rm=TRUE,value.var='val',drop=TRUE)

    mdfr<-rCompTCMs::getMDFR.CanonicalFormat(ddfr);

    if (verbose) cat("--finished rTCSAM02::getMDFR.Pop.CohortProgression(). \n");
    return(mdfr);
}
