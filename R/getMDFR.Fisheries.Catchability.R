#'
#'@title Get annual fishery catchabilities from model results from TCSAM02 model runs as a dataframe
#'
#'@description Function to get annual fishery catchabilities from TCSAM02 model runs as a dataframe.
#'
#'@param tcsams - single tcsam02.rep object, tcsam02.resLst object, or named list of the latter
#'@param cast - casting formula for excluding x,m,s factor levels from an average across unspecified factors
#'@param verbose - flag (T/F) to print debug info
#'
#'@return dataframe in canonical format
#'
#'@details Extracts annual fishery catchabilities.
#'
#'@export
#'
getMDFR.Fisheries.Catchability<-function(tcsams,cast="x",verbose=FALSE){
    if (verbose) cat("--starting rTCSAM02::getMDFR.Fisheries.Catchability().\n");
    options(stringsAsFactors=FALSE);

    path<-'mp/F_list/cpF_fyxms';
    mdfr<-getMDFR(path,tcsams,verbose);
    mdfr$fleet<-gsub("_"," ",mdfr$fleet,fixed=TRUE);#replace '_'s in fishery names with spaces

    castform<-"case+process+fleet+category+type+y&&cast~.";
    castform<-gsub("&&cast",paste0("+",cast),castform,fixed=TRUE);
    ddfr<-reshape2::dcast(mdfr,castform,fun.aggregate=mean,na.rm=TRUE,value.var='val',drop=TRUE)
    #ddfr[['.']]<-ifelse(ddfr[['.']]==0,NA,ddfr[['.']]);
    ddfr<-ddfr[!is.na(ddfr[['.']]),];#remove NA's

    mdfr<-getMDFR.CanonicalFormat(ddfr);

    if (verbose) cat("--rTCSAM02::getMDFR.Fisheries.Catchability() done. \n");
    return(mdfr);
}

