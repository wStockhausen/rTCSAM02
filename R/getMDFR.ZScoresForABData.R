#'
#'@title Get z-scores for abundance, biomass data from fits to data as dataframe
#'
#'@description Function to get z-scores for abundance, biomass data from fits to data as dataframe.
#'
#'@param afits -
#'@param verbose - flag (T/F) to print dagnostic info
#'
#'@return dataframe
#'
#'@details None.
#'
#'@export
#'
getMDFR.ZScoresForABData<-function(afits,
                                   verbose=FALSE){
    if (verbose) cat("---Running rTCSAM02::getMDFR.ZScoresForABData(...).\n");

    nf<-length(afits);
    if (verbose) cat("----number of fits =",nf,"\n");

    mdfr<-NULL;
    for (n in 1:nf){
        afit<-afits[[n]];
        if (!is.null(afit)){
            nll<-afit$nll;
            pdfType<-nll$nll.type;
            if (tolower(pdfType)!='none'){
                dfrp<-data.frame(x=afit$x,m=afit$m,s=afit$s,
                                 y=as.numeric(names(nll$zscrs)),
                                 val=nll$zscrs,var='zscr');
                mdfr<-rbind(mdfr,dfrp);
            }
        }
    }

    mdfr$x<-gsub("_"," ",tolower(mdfr$x),fixed=TRUE);
    mdfr$m<-gsub("_"," ",tolower(mdfr$m),fixed=TRUE);
    mdfr$s<-gsub("_"," ",tolower(mdfr$s),fixed=TRUE);

    if (verbose) cat("---Done running rTCSAM02::getMDFR.ZScoresForABData(...).\n");
    return(mdfr);
}
