#'
#'@title Get recruitment sex ratio from model results from TCSAM02 model runs as a dataframe
#'
#'@description Function to get recruitment sex ratio from model results from TCSAM2015 and rsimTCSAM model runs as a dataframe.
#'
#'@param tcsams - single TCSAM2015 model report object, or named list of such
#'@param rsims - single rsimTCSAM results object, or named list of such
#'@param verbose - flag (T/F) to print debug info
#'
#'@return dataframe with columns 'modeltype', 'model', 'pc', 'val'.
#'
#'@details Extracts recruitment sex ratio.
#'
#'@export
#'
getMDFR.Pop.SexRatio<-function(tcsams,rsims,verbose=FALSE){
    if (verbose) cat("--Getting recruitment sex ratio.\n");
    mdfr<-NULL;

    path<-'mp/R_list/Rx_c';
    mdfr<-getMDFR(path,tcsams,verbose);
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
    mdfr<-mdfr[,c('case','pc','y','val')];

    mdfr<-getMDFR.CanonicalFormat(mdfr);
    mdfr$type<-"population";
    mdfr$m<-"immature";
    mdfr$s<-"new shell";

    if (verbose) cat("--Done. \n");
    return(mdfr);
}
