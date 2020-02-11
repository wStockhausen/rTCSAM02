#'
#'@title Get model fits to effort data
#'
#'@description Function to get model fits to effort data.
#'
#'@param objs - single tcsam02 resLst object, or named list of them
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return dataframe
#'
#'@details Returned dataframe is in canonical format
#'
#'@export
#'
getMDFR.Fits.EffortData<-function(objs,
                                 verbose=FALSE){
    if (verbose) cat("--Starting rTCSAM02::getMDFR.Fits.EffortData().\n");
    options(stringsAsFactors=FALSE);

    mdfr<-NULL;
    if (class(objs)[1]=='list'){
        #objs should be a list of tcsam02 resLst objects
        for (nm in names(objs)){
            mdfrp<-getMDFR.Fits.EffortData(objs[[nm]],
                                          verbose=verbose);
            if (!is.null(mdfrp)) mdfrp$case<-nm;
            mdfr<-rbind(mdfr,mdfrp);
        }
    } else if (inherits(objs,'tcsam02.resLst')){
        #objs is a single tcsam02 resLst object
        gd<-objs$rep$model.fits$effortdata;
        ngd<-length(gd)-1;#last element is NULL
        if (ngd>0){
            nms.gd<-names(gd);
            for (n in 1:ngd){
                gdn<-gd[[n]];
                ngdn<-length(gdn)-1;
                if (ngdn>0){
                    #observed effort
                    arr<-getObj("obsEff",gdn);
                    mdfrp<-reshape2::melt(arr,value.name='val',as.is=TRUE);
                    mdfrp$fleet<-gdn$f;
                    mdfrp$pc<-gdn$n;
                    mdfrp$category<-'effort';
                    mdfrp$type<-'observed';
                    cis<-wtsUtilities::calcCIs(mdfrp$val,sdvs=gdn$stdv+0*mdfrp$val,pdfType="lognormal",ci=0.80);
                    mdfrp$lci<-cis$lci;
                    mdfrp$uci<-cis$uci;
                    mdfr<-rbind(mdfr,mdfrp);
                    #predicted effort
                    arr<-getObj("prdEff",gdn);
                    mdfrp<-reshape2::melt(arr,value.name='val',as.is=TRUE);
                    mdfrp$fleet<-gdn$f;
                    mdfrp$pc<-gdn$n;
                    mdfrp$category<-'effort';
                    mdfrp$type<-'predicted';
                    mdfrp$lci<-NA;
                    mdfrp$uci<-NA;
                    mdfr<-rbind(mdfr,mdfrp);
                    #z-scores
                    arr<-getObj("zscores",gdn);
                    mdfrp<-reshape2::melt(arr,value.name='val',as.is=TRUE);
                    mdfrp$fleet<-gdn$f;
                    mdfrp$pc<-gdn$n;
                    mdfrp$category<-'effort';
                    mdfrp$type<-'zscores';
                    mdfrp$lci<-NA;
                    mdfrp$uci<-NA;
                    mdfr<-rbind(mdfr,mdfrp);
                    #nlls
                    arr<-getObj("nlls",gdn);
                    mdfrp<-reshape2::melt(arr,value.name='val',as.is=TRUE);
                    mdfrp$fleet<-gdn$f;
                    mdfrp$pc<-gdn$n;
                    mdfrp$category<-'effort';
                    mdfrp$type<-'nlls';
                    mdfrp$y<-NA;
                    mdfrp$lci<-NA;
                    mdfrp$uci<-NA;
                    mdfr<-rbind(mdfr,mdfrp);
                }
            }#n
        }
        if (!is.null(mdfr)) {
            mdfr$case<-"tcsam02";
            mdfr<-mdfr[mdfr$val!=0,];#remove all entries with 0 val's
        }
    }

    if (!is.null(mdfr)) mdfr<-getMDFR.CanonicalFormat(mdfr);
    if (verbose) cat("--Finished rTCSAM02::getMDFR.Fits.EffortData().\n");
    return(mdfr);
}
