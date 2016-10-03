#'
#'@title Get natural mortality rates from model results from TCSAM02 model runs as a dataframe
#'
#'@description Function to get natural mortality rates from model results from TCSAM02 model runs as a dataframe.
#'
#'@param tcsams - single TCSAM02 model report object, or named list of such
#'@param type - flag indicating which M's to extract ('M_cxm', 'M_yxm' or 'M_yxmsz')
#'@param verbose - flag (T/F) to print debug info
#'
#'@return dataframe in canonical format
#'
#'@details Extracts natural mortality rates. Uses \code{reshape2} package.
#'
#'@export
#'
getMDFR.Pop.NaturalMortality<-function(tcsams=NULL,
                                      type=c('M_cxm','M_yxm','M_yxmsz'),
                                      verbose=FALSE){
    if (verbose) cat("--Getting natural mortality info\n");

    mdfr<-NULL;
    if (type[1]=='M_cxm'){
        mdfr<-getMDFR('mp/M_cxm',tcsams,verbose=verbose);
        mdfr$y<-'';
        if (inherits(tcsams,'tcsam02.rep')){tcsams<-list(tcsam=tcsams);}
        if (inherits(tcsams,'tcsam02.resLst')){tcsams<-list(tcsam=tcsams);}
        ums<-as.character(unique(mdfr$case));
        for (um in ums){
            tcsam<-tcsams[[um]];
            if (inherits(tcsam,'tcsam02.resLst')) tcsam<-tcsam$rep;
            pgi<-tcsam$mpi$nm$pgi;
            nPCs<-length(pgi$pcs)-1;
            for (pc in 1:nPCs){
                idx<-(mdfr$pc==pc)&(mdfr$case==um);
                mdfr$y[idx]<-pgi$pcs[[pc]]$YEAR_BLOCK;
                mdfr$y[idx]<-reformatTimeBlocks(mdfr$y[idx],tcsam$mc$dims)
            }
        }
        mdfr<-mdfr[,c("case","pc","y","x","m","val")];
    } else if (type[1]=='M_yxm'){
        mdfr<-getMDFR('mp/M_yxmsz',tcsams,verbose=verbose);
        mdfr<-reshape2::dcast(mdfr,formula='case+y+x+m~.',fun.aggregate=mean,value.var='val');
        names(mdfr)<-c("case","y","x","m","val");
    } else if (type[1]=='M_yxmsz'){
        mdfr<-getMDFR('mp/M_yxmsz',tcsams,verbose=verbose);
        mdfr<-mdfr[,c("case","y","x","m","s","z","val")];
    }

    mdfr<-getMDFR.CanonicalFormat(mdfr);
    mdfr$type<-"population";

    if (verbose) cat("--Done. \n");
    return(mdfr);
}
