#'
#'@title Get population maturity ogives by year from model results from TCSAM02 model runs as a dataframe
#'
#'@description Function to get population maturity ogives by year from model results from TCSAM02 model runs as a dataframe.
#'
#'@param tcsams - single tcsam02.rep object, tcsam02.resLst object, or named list of the latter
#'@param verbose - flag (T/F) to print debug info
#'
#'@return dataframe in canonical format
#'
#'@details Extracts the estimated population maturity ogives by year.
#'
#'@export
#'
getMDFR.Pop.MaturityOgives<-function(tcsams,verbose=FALSE){
    if (verbose) cat("--starting rTCSAM02::getMDFR.Pop.MaturityOgives().\n");
    options(stringsAsFactors=FALSE);

    path<-'mr/P_list/prM_yz';
    mdfr<-getMDFR(path,tcsams,verbose);
    mdfr$process<-"population";
    mdfr$type<-'predicted';
    mdfr<-removeImmOS(mdfr);

    mdfr<-rCompTCMs::getMDFR.CanonicalFormat(mdfr);

    if (verbose) cat("--finished rTCSAM02::getMDFR.Pop.MaturityOgives(). \n");
    return(mdfr);
}
