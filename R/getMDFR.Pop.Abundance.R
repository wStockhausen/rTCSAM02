#'
#'@title Get population abundance time series from model results from TCSAM02 model runs as a dataframe
#'
#'@description Function to get population abundance time series from model results from TCSAM02 model runs as a dataframe.
#'
#'@param tcsams - single tcsam02.rep object, tcsam02.resLst object, or named list of the latter
#'@param cast - casting formula for excluding y,x,m,s,z factor levels from an average-at-size across unspecified factors
#'@param verbose - flag (T/F) to print debug info
#'
#'@return dataframe in canonical format
#'
#'@details Extracts the estimated population abundance time series.
#'
#'@export
#'
getMDFR.populations.Abundance<-function(tcsams,cast="y+x",verbose=FALSE){
    if (verbose) cat("--rTCSAM02::Getting population abundance time series.\n");

    path<-'mr/S_list/N_vyxmsz';
    mdfr<-getMDFR(path,tcsams,verbose);
    mdfr$fleet<-gsub("_"," ",mdfr$fleet,fixed=TRUE);#replace '_'s in population names with spaces
    mdfr$process<-"population";
    mdfr$type<-'predicted';
    mdfr<-removeImmOS(mdfr);

    castform<-"case+process+fleet+category+type+pc&&cast~.";
    castform<-gsub("&&cast",paste0("+",cast),castform,fixed=TRUE);
    ddfr<-reshape2::dcast(mdfr,castform,fun.aggregate=mean,na.rm=TRUE,value.var='val',drop=TRUE)
    ddfr[['.']]<-ifelse(ddfr[['.']]==0,NA,ddfr[['.']]);
    ddfr<-ddfr[!is.na(ddfr[['.']]),];#remove NA's

    mdfr<-rCompTCMs::getMDFR.CanonicalFormat(ddfr);

    if (verbose) cat("--Done. \n");
    return(mdfr);
}
