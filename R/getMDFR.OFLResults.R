#'
#'@title Get OFL results as a dataframe
#'
#'@description Function to get a dataframe of OFL results.
#'
#'@param objs - single tcsam02 resLst object, or named list of them
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return dataframe
#'
#'@details Returned dataframe is NOT in canonical format.
#'
#'@import dplyr
#'@import magrittr
#'
#'@export
#'
getMDFR.OFLResults<-function(objs,
                             verbose=FALSE){
    if (verbose) cat("--Starting rTCSAM02::getMDFR.OFLResults().\n");
    options(stringsAsFactors=FALSE);

    cols<-c("OFL","Fofl","prjB","curB","Fmsy","Bmsy","MSY","B100","avgRecM","avgRecF");

    mdfr<-NULL;
    if ((class(objs)[1]=='list')&&inherits(objs[[1]],"tcsam02.resLst")){
        #objs should be a list of tcsam02 resLst objects
        for (nm in names(objs)){
            mdfrp<-getMDFR.OFLResults(objs[[nm]],
                                          verbose=verbose);
            if (!is.null(mdfrp)) mdfrp$case<-nm;
            mdfr<-rbind(mdfr,mdfrp);
        }
    } else if (inherits(objs,'tcsam02.resLst')){
        #objs is a single tcsam02 resLst object
        numPars = nrow(getMDFR.ParameterValues(objs) %>% dplyr::filter(phase>0));
        objFun  = objs$rep$objFun;
        maxGrd  = objs$rep$maxGrad;
        hasSEs  = !is.null(objs$std);
        ofl<-objs$rep$ptrOFLResults;
        if (!is.null(ofl)){
            mdfr<-as.data.frame(list(case="",numPars=numPars,objFun=objFun,maxGrad=maxGrd,hasSEs=hasSEs,ofl[cols]));
        }
    } else if ((class(objs)[1]=='list')&&inherits(objs[[1]],"tcsam02.rep")){
        #objs should be a list of tcsam02 repLst objects
        for (nm in names(objs)){
            mdfrp<-getMDFR.OFLResults(objs[[nm]],
                                          verbose=verbose);
            if (!is.null(mdfrp)) mdfrp$case<-nm;
            mdfr<-rbind(mdfr,mdfrp);
        }
    } else if (inherits(objs,'tcsam02.rep')){
        #objs is a single tcsam02 rep object
        numPars = nrow(getMDFR.ParameterValues(objs) %>% dplyr::filter(phase>0));
        objFun  = objs$objFun;
        maxGrd  = objs$maxGrad;
        hasSEs  = NA;
        ofl<-objs$ptrOFLResults;
        if (!is.null(ofl)){
            mdfr<-as.data.frame(list(case="",numPars=numPars,objFun=objFun,maxGrad=maxGrd,hasSEs=hasSEs,ofl[cols]));
        }
    } else {
        cat("---getMDFR.OFLResults: ERROR!\n");
        cat("getMDFR.OFLResults: unrecognized input\n");
        cat("'objs' should be a tcsam02.resLst or tcsam02.rep object, or lists thereof.\n")
        return(NULL)
    }

    if (verbose) cat("--Finished rTCSAM02::getMDFR.OFLResults().\n");
    return(mdfr);
}
