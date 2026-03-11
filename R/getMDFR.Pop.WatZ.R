#'
#'@title Get individual weights-at-size from TCSAM02 model runs as a dataframe
#'
#'@description Function to get individual weights-at-size from TCSAM02 model runs as a dataframe.
#'
#'@param tcsams - single tcsam02.rep object, tcsam02.resLst object, or named list of the latter
#'@param verbose - flag (T/F) to print debug info
#'
#'@return dataframe in canonical format
#'
#'@details Extracts the individual weights-at-size by sex, maturity state, and size. Units are in kg.
#'
#'@export
#'
getMDFR.Pop.WatZ<-function(tcsams,verbose=FALSE){
    if (verbose) cat("--starting rTCSAM02::getMDFR.Pop.WatZ().\n");
    options(stringsAsFactors=FALSE);

    path<-'mp/wAtZ_xmz';
    mdfr<-getMDFR(path,tcsams,verbose);
    mdfr$process<-"population";
    mdfr$type<-'constants';     #--based on inputs, so not really predicted
    mdfr<-removeImmOS(mdfr);

    mdfr<-getMDFR.CanonicalFormat(mdfr);

    if (verbose) cat("--finished rTCSAM02::getMDFR.Pop.WatZ(). \n");
    return(mdfr);
}
