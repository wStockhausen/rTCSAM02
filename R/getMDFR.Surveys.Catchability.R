#'
#'@title Get annual survey catchabilities from model results from TCSAM02 model runs as a dataframe
#'
#'@description Function to get annual survey catchabilities from TCSAM02 model runs as a dataframe.
#'
#'@param tcsams - single tcsam02.rep object, tcsam02.resLst object, or named list of the latter
#'@param cast - casting formula for excluding x,m,s factor levels from an average across unspecified factors
#'@param verbose - flag (T/F) to print debug info
#'
#'@return dataframe in canonical format
#'
#'@details Extracts annual survey catchabilities.
#'
#'@export
#'
getMDFR.Surveys.Catchability<-function(tcsams,cast="x",verbose=FALSE){
    if (verbose) cat("--starting rTCSAM02::getMDFR.Surveys.Catchability().\n");
    options(stringsAsFactors=FALSE);

    path<-'mp/S_list/Q_vyxms';
    mdfr<-getMDFR(path,tcsams,verbose);
    mdfr$fleet<-gsub("_"," ",mdfr$fleet,fixed=TRUE);#replace '_'s in survey names with spaces

    castform<-"case+process+fleet+category+type+y&&cast~.";
    castform<-gsub("&&cast",paste0("+",cast),castform,fixed=TRUE);
    ddfr<-reshape2::dcast(mdfr,castform,fun.aggregate=mean,na.rm=TRUE,value.var='val',drop=TRUE)
    ddfr[['.']]<-ifelse(ddfr[['.']]==0,NA,ddfr[['.']]);
    ddfr<-ddfr[!is.na(ddfr[['.']]),];#remove NA's

    mdfr<-getMDFR.CanonicalFormat(ddfr);

    if (verbose) cat("--rTCSAM02::getMDFR.Surveys.Catchability() done. \n");
    return(mdfr);
}

