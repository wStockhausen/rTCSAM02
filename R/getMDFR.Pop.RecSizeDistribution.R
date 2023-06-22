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
#'@details Extracts recruitment size distributions (values are *not* scaled by bin size).
#'
#'@export
#'
getMDFR.Pop.RecSizeDistribution<-function(tcsams,verbose=FALSE){
    if (verbose) cat("--rTCSAM02::getMDFR.Pop.RecSizeDistribution: Getting recruitment size distribution.\n");
    options(stringsAsFactors=FALSE);

    mdfr<-NULL;
    mdfr<-getMDFR('mp/R_list/R_cz',tcsams,verbose);
    mdfr$y<-'';
    ums<-as.character(unique(mdfr$case));

    if (inherits(tcsams,'tcsam02.rep')){tcsams<-list(tcsam=tcsams);}
    if (inherits(tcsams,'tcsam02.resLst')){tcsams<-list(tcsam=tcsams);}

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

    if (verbose) cat("--rTCSAM02::getMDFR.Pop.RecSizeDistribution: Done. \n");
    return(mdfr);
}
