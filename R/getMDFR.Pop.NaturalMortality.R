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
#''type' can be one of:
#' * M_c
#' * M_cxm
#' * M_cxms
#' * M_yxm
#' * M_yxmsz
#' * M_cy
#'
#'@export
#'
#'@md
#'
getMDFR.Pop.NaturalMortality<-function(tcsams=NULL,
                                      type='M_c',
                                      verbose=FALSE){
    if (verbose) cat("--rTCSAM02::getMDFR.Pop.NaturalMortality: Getting natural mortality info\n");
    options(stringsAsFactors=FALSE);

    mdfr<-NULL;
    if (type[1] %in% c('M_c','M_cx','M_cxm','M_cxms')){
        mdfr<-rTCSAM02::getMDFR('mp/M_c',tcsams,verbose=verbose);
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
                mdfr$x[idx]<-pgi$pcs[[pc]]$SEX;
                mdfr$m[idx]<-pgi$pcs[[pc]]$MATURITY_STATE;
                mdfr$s[idx]<-pgi$pcs[[pc]]$SHELL_CONDITION;
            }
        }
        mdfr<-mdfr[,c("case","pc","y","x","m","s","val")];
    } else if (type[1]=='M_yxm'){
        mdfr<-rTCSAM02::getMDFR('mp/M_yxmsz',tcsams,verbose=verbose);
        mdfr<-reshape2::dcast(mdfr,formula='case+y+x+m~.',fun.aggregate=mean,value.var='val');
        names(mdfr)<-c("case","y","x","m","val");
    } else if (type[1]=='M_yxmsz'){
        mdfr<-rTCSAM02::getMDFR('mp/M_yxmsz',tcsams,verbose=verbose);
        mdfr<-mdfr[,c("case","y","x","m","s","z","val")];
    } else if (type[1] %in% c('M_cy')){
        mdfr<-rTCSAM02::getMDFR('mp/M_cy',tcsams,verbose=verbose);
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
                mdfr$x[idx]<-pgi$pcs[[pc]]$SEX;
                mdfr$m[idx]<-pgi$pcs[[pc]]$MATURITY_STATE;
                mdfr$s[idx]<-pgi$pcs[[pc]]$SHELL_CONDITION;
            }
        }
        mdfr<-mdfr[,c("case","pc","y","x","m","s","val")];
    }

    mdfr<-rTCSAM02::getMDFR.CanonicalFormat(mdfr);
    mdfr$process<-"population";

    if (verbose) cat("--rTCSAM02::getMDFR.Pop.NaturalMortality: Done. \n");
    return(mdfr);
}
