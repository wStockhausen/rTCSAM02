#'
#'@title Get survey biomass time series from model results from TCSAM02 model runs as a dataframe
#'
#'@description Function to get survey biomass time series from model results from TCSAM02 model runs as a dataframe.
#'
#'@param tcsams - single tcsam02.rep object, tcsam02.resLst object, or named list of the latter
#'@param category - 'index' is only choice
#'@param cast - casting formula for excluding x,m,s,z factor levels from an average-at-size across unspecified factors
#'@param verbose - flag (T/F) to print debug info
#'
#'@return dataframe in canonical format
#'
#'@details Extracts the estimated survey biomass time series.
#'
#'@export
#'
getMDFR.Surveys.Biomass<-function(tcsams,category='index',cast="x",verbose=FALSE){
    if (verbose) cat("--starting rTCSAM02::getMDFR.Surveys.Biomass().\n");
    options(stringsAsFactors=FALSE);

    category<-category[1];

    path<-'mr/S_list/B_vyxmsz';
    mdfr<-getMDFR(path,tcsams,verbose);
    mdfr$fleet<-gsub("_"," ",mdfr$fleet,fixed=TRUE);#replace '_'s in survey names with spaces
    mdfr$category<-category;
    mdfr$type<-'predicted';
    mdfr<-removeImmOS(mdfr);

    castform<-"case+process+fleet+category+type+pc+y";
    if (!is.null(cast)|(cast!='')) castform<-paste0(castform,"+",cast);
    castform<-paste0(castform,"~.");
    ddfr<-reshape2::dcast(mdfr,castform,fun.aggregate=sum,na.rm=TRUE,value.var='val',drop=TRUE)
    ddfr[['.']]<-ifelse(ddfr[['.']]==0,NA,ddfr[['.']]);
    ddfr<-ddfr[!is.na(ddfr[['.']]),];#remove NA's

    mdfr<-rCompTCMs::getMDFR.CanonicalFormat(ddfr);

    if (verbose) cat("--finished rTCSAM02::getMDFR.Surveys.Biomass(). \n");
    return(mdfr);
}
