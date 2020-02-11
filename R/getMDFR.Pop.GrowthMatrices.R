#'@title Get growth transition matrices from model results from TCSAM02 model runs as a dataframe
#'
#'@description Function to get growth transition matrices from model results from TCSAM02 model runs as a dataframe.
#'
#'@param tcsams - single tcsam02.rep, tcsam02.resLst, named list of the latter, or general list
#'@param path - path into tcsams object at which to find the transition arrays
#'@param verbose - flag (T/F) to print debug info
#'
#'@return dataframe in canonical format.
#'Note that 'z' is pre-molt size, 'zp' is post-molt size.
#'
#'@details Extracts growth transition matrices.
#'
#'@export
#'
getMDFR.Pop.GrowthMatrices<-function(tcsams,
                                     path='mp/T_list/T_czz',
                                     verbose=FALSE){
    if (verbose) cat("--rTCSAM02::Getting growth transition matrices.\n");
    options(stringsAsFactors=FALSE);

    mdfr<-NULL;
    if (path!='mp/T_list/T_czz'){
        obj<-getObj(path,tcsams,verbose=verbose);
        if (!is.null(obj)){
            mdfr<-reshape2::melt(obj,value.name='val',as.is=TRUE);
            mdfr$case<-'tcsam';
            #in mdfr above, 'z' is post-molt size, 'zp' is pre-molt size
            #"transpose" indices so 'z' represents pre-molt size, 'zp' post-molt size
            zp<-mdfr$z;
            mdfr$z<-mdfr$zp;
            mdfr$zp<-zp;
            mdfr<-getMDFR.CanonicalFormat(mdfr);
            mdfr$process<-"population";
            mdfr$m<-"immature";
            mdfr$s<-"all";
        }
    } else {
      mdfr<-getMDFR(path,tcsams,verbose);
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
      mdfr<-mdfr[,c('case','pc','y','x','m','s','z','zp','val')];

      #in mdfr above, 'z' is post-molt size, 'zp' is pre-molt size
      #"transpose" indices so 'z' represents pre-molt size, 'zp' post-molt size
      zp<-mdfr$z;
      mdfr$z<-mdfr$zp;
      mdfr$zp<-zp;

      mdfr<-getMDFR.CanonicalFormat(mdfr);
      mdfr$process<-"population";
      mdfr$m<-"immature";
      mdfr$s<-"all";
    }

    if (verbose) cat("--rTCSAM02::Done. \n");
    return(mdfr);
}
