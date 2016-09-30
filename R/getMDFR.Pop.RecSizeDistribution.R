#'
#'@title Get recruitment size distribution from model results from TCSAM02 model runs as a dataframe
#'
#'@description Function to get recruitment size distribution from model results from TCSAM02 model runs as a dataframe.
#'
#'@param tcsams - single tcsam02.rep object, tcsam02.resLst object, or named list of the latter
#'@param verbose - flag (T/F) to print debug info
#'
#'@return dataframe in canonical format.
#'
#'@details Extracts mean growth increments.
#'
#'@export
#'
getMDFR.Pop.RecSizeDistribution<-function(tcsams,verbose=FALSE){
    if (verbose) cat("--Getting recruitment size distribution.\n");

    mdfr<-getMDFR('mp/R_list/R_cz',tcsams,verbose);
    mdfr$y<-'';
    ums<-as.character(unique(mdfr$case))
    for (um in ums){
        tcsam<-tcsams[[um]];
        if (inherits(tcsam,'tcsam02.resLst')) tcsam<-tcsam$rep;
        pgi<-tcsam$mpi$rec$pgi;
        nPCs<-length(pgi$pcs)-1;#last element is a NULL
        for (pc in 1:nPCs){
            idx<-(mdfr$pc==pc)&(mdfr$case==um);
            mdfr$y[idx]<-pgi$pcs[[pc]]$YEAR_BLOCK;
            mdfr$y[idx]<-reformatTimeBlocks(mdfr$y[idx],tcsam$mc$dims);
        }
    }
    mdfr<-mdfr[,c('case','pc','y','z','val')];

    mdfr<-getMDFR.CanonicalFormat(mdfr);
    mdfr$type<-"population";
    mdfr$m<-"immature";
    mdfr$s<-"new shell";

    if (verbose) cat("--Done. \n");
    return(mdfr);
}
