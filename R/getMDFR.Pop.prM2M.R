#'
#'@title Get molt-to-maturity ogives from model results from TCSAM02 model runs as a dataframe
#'
#'@description Function to get molt-to-maturity ogives from model results from TCSAM02 model runs as a dataframe.
#'
#'@param tcsams - single tcsam02.rep object, tcsam02.resLst object, named list of the latter, or general list
#'@param path - path into tcsams object at which to find the prM2M array
#'@param verbose - flag (T/F) to print debug info
#'
#'@return dataframe in canonical format
#'
#'@details Extracts molt-to-maturity ogives.
#'
#'@export
#'
getMDFR.Pop.PrM2M<-function(tcsams,
                            path='mp/prM2M_cz',
                            verbose=FALSE){
    if (verbose) cat("--rTCSAM02::getMDFR.Pop.PrM2M().\n");
    options(stringsAsFactors=FALSE);

    mdfr<-NULL;
    if (path!='mp/prM2M_cz'){
        obj<-getObj(path,tcsams,verbose=verbose);
        if (!is.null(obj)){
            mdfr<-reshape2::melt(obj,value.name='val',as.is=TRUE);
            mdfr$case<-'tcsam';
            mdfr<-rCompTCMs::getMDFR.CanonicalFormat(mdfr);
            mdfr$process<-"population";
            mdfr$m<-"immature";
            mdfr$s<-"all";
        }
    } else {
      mdfr<-getMDFR('mp/prM2M_cz',tcsams,verbose);
      if (is.null(mdfr)) mdfr<-getMDFR('mp/prMolt2Mat_cz',tcsams,verbose);

      mdfr$y<-'';
      mdfr$x<-'';

      if (inherits(tcsams,'tcsam02.rep')){tcsams<-list(tcsam=tcsams);}
      if (inherits(tcsams,'tcsam02.resLst')){tcsams<-list(tcsam=tcsams);}

      ums<-as.character(unique(mdfr$case));
      for (um in ums){
          tcsam<-tcsams[[um]];
          if (inherits(tcsam,'tcsam02.resLst')) tcsam<-tcsam$rep;
          pgi<-tcsam$mpi$mat$pgi;
          nPCs<-length(pgi$pcs)-1;#last element is a NULL
          for (pc in 1:nPCs){
              idx<-(mdfr$pc==pc)&(mdfr$case==um);
              mdfr$x[idx]<-tolower(pgi$pcs[[pc]]$SEX);
              mdfr$y[idx]<-pgi$pcs[[pc]]$YEAR_BLOCK;
              mdfr$y[idx]<-reformatTimeBlocks(mdfr$y[idx],tcsam$mc$dims);
          }
      }
      mdfr<-mdfr[,c('case','pc','y','x','z','val')];

      mdfr<-rCompTCMs::getMDFR.CanonicalFormat(mdfr);
      mdfr$process<-"population";
      mdfr$m<-"immature";
      mdfr$s<-"all";
    }

    if (verbose) cat("--Done. \n");
    return(mdfr);
}
