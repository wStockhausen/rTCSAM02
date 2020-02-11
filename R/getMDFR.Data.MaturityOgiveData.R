#'
#'@title Get maturity ogive data as a dataframe
#'
#'@description Function to get maturity ogive data as a dataframe.
#'
#'@param objs - single model report list object, or named list of them
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return dataframe (or NULL)
#'
#'@details Returned dataframe is in canonical format
#'
#'@export
#'
getMDFR.Data.MaturityOgiveData<-function(objs,
                                 verbose=FALSE){
    if (verbose) cat("--Starting getMDFR.Data.MaturityOgiveData().\n");
    options(stringsAsFactors=FALSE);

    mdfr<-NULL;
    if (class(objs)[1]=='list'){
        #repObjs should be a list of tcsam02 resLst objects
        for (nm in names(objs)){
            mdfrp<-getMDFR.Data.MaturityOgiveData(objs[[nm]],
                                          verbose=verbose);
            if (!is.null(mdfrp)) mdfrp$case<-nm;
            mdfr<-rbind(mdfr,mdfrp);
        }
    } else if (inherits(objs,'tcsam02.resLst')){
        #objs is a single tcsam02 resLst object
        gd<-objs$rep$data$maturityogives;
        ngd<-length(gd);
        if (ngd>0){
            nms.gd<-names(gd);
            if (verbose) cat("--element names:",paste(nms.gd,collapse=" "),"\n")
            for (nm in nms.gd){
                gdn<-gd[[nm]];
                if (!is.null(gdn)){
                    nms.gdn<-names(gdn);
                    if (verbose) cat("----element names:",paste(nms.gdn,collapse=" "),"\n")
                    mdfrp<-data.frame(x=gdn$sex,fleet=gdn$survey,
                                      category=nm,type="observed",
                                      y=gdn$data[,"y"],z=gdn$data[,"z"],val=gdn$data[,"fraction mature"]);
                    idx<-order(mdfrp$type,mdfrp$y,mdfrp$z,mdfrp$val);
                    mdfr<-rbind(mdfr,mdfrp[idx,]);
                }
            }#n
        }
        if (!is.null(mdfr)) mdfr$case<-"tcsam02";
    }

    if (!is.null(mdfr)) mdfr<-getMDFR.CanonicalFormat(mdfr);
    if (verbose) cat("--Finished getMDFR.Data.MaturityOgiveData().\n");
    return(mdfr);
}
