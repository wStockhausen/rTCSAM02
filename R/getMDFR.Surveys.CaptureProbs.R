#'
#'@title Get survey capture probability (Q_vyxmsz) functions from TCSAM02 model runs as a dataframe
#'
#'@description Function to get survey scapture probability (Q_vyxmsz) functions from TCSAM02 model runs as a dataframe.
#'
#'@param tcsams - single tcsam02.rep object, tcsam02.resLst object, or named list of the latter
#'@param verbose - flag (T/F) to print debug info
#'@param cast - casting formula for excluding y,x,m,s factor levels from an average-at-size across unspecified factors
#'
#'@return dataframe in canonical format
#'
#'@details Extracts survey capture probability functions (Q_vyxmsz).
#'
#'@export
#'
getMDFR.Surveys.CaptureProbs<-function(tcsams,verbose=FALSE,cast="y+x"){
    if (verbose) cat("--rTCSAM02::getMDFR.Surveys.CaptureProbs()\n");
    options(stringsAsFactors=FALSE);

    path<-'mp/S_list/Q_vyxmsz';
    mdfr<-getMDFR(path,tcsams,verbose);
    mdfr$fleet<-gsub("_"," ",mdfr$fleet,fixed=TRUE);#replace '_'s in survey names with spaces
    mdfr$z<-as.numeric(mdfr$z);

    castform<-"case+process+fleet+category+type+pc&&cast+z~.";
    castform<-gsub("&&cast",paste0("+",cast),castform,fixed=TRUE);
    ddfr<-reshape2::dcast(mdfr,castform,fun.aggregate=mean,na.rm=TRUE,value.var='val',drop=TRUE)
    ddfr[['.']]<-ifelse(ddfr[['.']]==0,NA,ddfr[['.']]);
    ddfr<-ddfr[!is.na(ddfr[['.']]),];#remove NA's

    mdfr<-getMDFR.CanonicalFormat(ddfr);

    if (verbose) cat("--Done. \n");
    return(mdfr);
}
