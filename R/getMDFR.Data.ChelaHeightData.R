#'
#'@title Get chela height data as a dataframe
#'
#'@description Function to get chela height data as a dataframe.
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
getMDFR.Data.ChelaHeightData<-function(objs,
                                 verbose=FALSE){
    if (verbose) cat("--Starting getMDFR.Data.ChelaHeightData().\n");
    options(stringsAsFactors=FALSE);

    mdfr<-NULL;
    if (class(objs)[1]=='list'){
        #repObjs should be a list of tcsam02 resLst objects
        for (nm in names(objs)){
            mdfrp<-getMDFR.Data.ChelaHeightData(objs[[nm]],
                                          verbose=verbose);
            if (!is.null(mdfrp)) mdfrp$case<-nm;
            mdfr<-rbind(mdfr,mdfrp);
        }
    } else if (inherits(objs,'tcsam02.resLst')){
        #objs is a single tcsam02 resLst object
        gd<-objs$rep$data$chelaheight;
        ngd<-length(gd);
        if (ngd>0){
            nms.gd<-names(gd);
            if (verbose) cat("--element names:",paste(nms.gd,collapse=" "),"\n")
            for (nm in nms.gd){
                gdn<-gd[[nm]];
                if (!is.null(gdn)){
                    nms.gdn<-names(gdn);
                    if (verbose) cat("----element names:",paste(nms.gdn,collapse=" "),"\n")
                    for (x in c('MALE','FEMALE')){
                        gdnx<-gdn[[x]];
                        if (!is.null(gdnx)){
                           mdfrp<-data.frame(x=x,m="immature",s="new shell",
                                              category=nm,type="observed",
                                              y=gdnx[,"y"],z=gdnx[,"z"],val=gdnx[,"zp"]);
                            idx<-order(mdfrp$type,mdfrp$y,mdfrp$z,mdfrp$val);
                            mdfr<-rbind(mdfr,mdfrp[idx,]);
                        }
                    }#x
                }
            }#n
        }
        if (!is.null(mdfr)) mdfr$case<-"tcsam02";
    }

    if (!is.null(mdfr)) mdfr<-rCompTCMs::getMDFR.CanonicalFormat(mdfr);
    if (verbose) cat("--Finished getMDFR.Data.ChelaHeightData().\n");
    return(mdfr);
}
