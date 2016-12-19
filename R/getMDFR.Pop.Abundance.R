#'
#'@title Get population abundance time series from model results from TCSAM02 model runs as a dataframe
#'
#'@description Function to get population abundance time series from model results from TCSAM02 model runs as a dataframe.
#'
#'@param tcsams - single tcsam02.rep object, tcsam02.resLst object, or named list of the latter
#'@param cast - casting formula for excluding x,m,s,z factor levels from a sum across the unspecified factors
#'@param verbose - flag (T/F) to print debug info
#'
#'@return dataframe in canonical format
#'
#'@details Extracts the estimated population abundance time series.
#'
#'@export
#'
getMDFR.Pop.Abundance<-function(tcsams,cast="x",verbose=FALSE){
    if (verbose) cat("--starting rTCSAM02::getMDFR.Pop.Abundance().\n");
    options(stringsAsFactors=FALSE);

    path<-'mr/P_list/N_yxmsz';
    mdfr<-getMDFR(path,tcsams,verbose);
    mdfr$process<-"population";
    mdfr$type<-'predicted';
    mdfr<-removeImmOS(mdfr);

    castform<-"case+process+fleet+category+type+pc+y";
    if (!is.null(cast)|(cast!='')) castform<-paste0(castform,"+",cast);
    castform<-paste0(castform,"~.");
    if (verbose) cat("casting formula = '",castform,"'\n",sep='')
    ddfr<-reshape2::dcast(mdfr,castform,fun.aggregate=sum,na.rm=TRUE,value.var='val',drop=TRUE)
    ddfr[['.']]<-ifelse(ddfr[['.']]==0,NA,ddfr[['.']]);
    ddfr<-ddfr[!is.na(ddfr[['.']]),];#remove NA's

    mdfr<-rCompTCMs::getMDFR.CanonicalFormat(ddfr);

    if (verbose) cat("--finished rTCSAM02::getMDFR.Pop.Abundance(). \n");
    return(mdfr);
}
