#'
#'@title Get survey maturity ogives by year from model results from TCSAM02 model runs as a dataframe
#'
#'@description Function to get survey maturity ogives by year from model results from TCSAM02 model runs as a dataframe.
#'
#'@param tcsams - single tcsam02.rep object, tcsam02.resLst object, or named list of the latter
#'@param category - 'index' is only choice
#'@param verbose - flag (T/F) to print debug info
#'
#'@return dataframe in canonical format
#'
#'@details Extracts the estimated survey maturity ogives by year.
#'
#'@export
#'
getMDFR.Surveys.MaturityOgives<-function(tcsams,category='index',verbose=FALSE){
    if (verbose) cat("--starting rTCSAM02::getMDFR.Surveys.MaturityOgives().\n");
    options(stringsAsFactors=FALSE);

    category<-'index';

    path<-'mr/S_list/prM_vyz';
    mdfr<-getMDFR(path,tcsams,verbose);
    mdfr$fleet<-gsub("_"," ",mdfr$fleet,fixed=TRUE);#replace '_'s in survey names with spaces
    mdfr$category<-category;
    mdfr$type<-'predicted';
    mdfr<-removeImmOS(mdfr);

    mdfr<-getMDFR.CanonicalFormat(mdfr);

    if (verbose) cat("--finished rTCSAM02::getMDFR.Surveys.MaturityOgives(). \n");
    return(mdfr);
}
