#'
#'@title Get mean growth increments from model results from TCSAM2015 and rsimTCSAM model runs as a dataframe
#'
#'@description Function to get mean growth increments from model results from TCSAM2015 and rsimTCSAM model runs as a dataframe.
#'
#'@param tcsams - single TCSAM2015 model report object, or named list of such
#'@param verbose - flag (T/F) to print debug info
#'
#'@return dataframe in canonical format
#'
#'@details Extracts mean growth increments.
#'
#'@export
#'
getMDFR.Pop.MeanGrowth<-function(tcsams,verbose=FALSE){
    if (verbose) cat("--rTCSAM02::Getting mean growth increments\n");

    mdfr<-NULL;
    mdfr<-getMDFR('mp/T_list/mnZAM_cz',tcsams,verbose);
    mdfr$y<-'';
    mdfr$x<-'';

    if (inherits(tcsams,'tcsam02.rep')){tcsams<-list(tcsam=tcsams);}
    if (inherits(tcsams,'tcsam02.resLst')){tcsams<-list(tcsam=tcsams);}

    ums<-as.character(unique(mdfr$case))
    for (um in ums){
        tcsam<-tcsams[[um]];
        if (inherits(tcsam,'tcsam02.resLst')) tcsam<-tcsam$rep;
        pgi<-tcsam$mpi$grw$pgi;
        nPCs<-length(pgi$pcs)-1;#last element is a NULL
        for (pc in 1:nPCs){
            idx<-(mdfr$pc==pc)&(mdfr$case==um);
            mdfr$x[idx]<-tolower(pgi$pcs[[pc]]$SEX);
            mdfr$y[idx]<-pgi$pcs[[pc]]$YEAR_BLOCK;
            mdfr$y[idx]<-reformatTimeBlocks(mdfr$y[idx],tcsam$mc$dims);
        }
    }
    mdfr<-mdfr[,c('case','pc','y','x','m','s','z','val')];

    mdfr<-rCompTCMs::getMDFR.CanonicalFormat(mdfr);
    mdfr$process<-"population";
    mdfr$m<-"immature";
    mdfr$s<-"all";

    if (verbose) cat("--rTCSAM02::Done. \n");
    return(mdfr);
}
