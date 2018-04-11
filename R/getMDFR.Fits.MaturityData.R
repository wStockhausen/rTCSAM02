#'
#'@title Get model fits to maturity data
#'
#'@description Function to get model fits to maturity data.
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
getMDFR.Fits.MaturityData<-function(objs,
                                 verbose=FALSE){
    if (verbose) cat("--Starting getMDFR.Fits.MaturityData().\n");
    options(stringsAsFactors=FALSE);

    mdfr<-NULL;
    if (class(objs)[1]=='list'){
        #repObjs should be a list of tcsam02 resLst objects
        for (nm in names(objs)){
            mdfrp<-getMDFR.Fits.MaturityData(objs[[nm]],
                                          verbose=verbose);
            if (!is.null(mdfrp)) mdfrp$case<-nm;
            mdfr<-rbind(mdfr,mdfrp);
        }
    } else if (inherits(objs,'tcsam02.resLst')){
        #objs is a single tcsam02 resLst object
        md<-objs$rep$model.fits$maturitydata;
        nmd<-length(md)-1;#last element is NULL
        if (nmd>0){
            nms.md<-names(md);
            for (n in 1:nmd){
                mdn<-md[[n]];
                nmdn<-length(mdn)-1;
                if (nmdn>0){
                    ctg<-gsub("_"," ",nms.md[n],fixed=TRUE);
                        x<-'male';
                        dfrp1<-data.frame(x=x,m="",s="new shell",
                                          category=ctg,type="observed",
                                          y=mdn$y,z=mdn$z,val=mdn$obsPM);
                        dfrp2<-data.frame(x=x,m="",s="new shell",
                                          category=ctg,type="predicted",
                                          y=mdn$y,z=mdn$z,val=mdn$modPM);
                        dfrp3<-data.frame(x=x,m="",s="new shell",
                                          category=ctg,type="nlls",
                                          y=mdn$y,z=mdn$z,val=mdn$nlls);
                        dfrp4<-data.frame(x=x,m="",s="new shell",
                                          category=ctg,type="zscores",
                                          y=mdn$y,z=mdn$z,val=mdn$zscrs);
                        mdfrp<-rbind(dfrp1,dfrp2,dfrp3,dfrp4);
                        idx<-order(mdfrp$type,mdfrp$y,mdfrp$z,mdfrp$val);
                        mdfr<-rbind(mdfr,mdfrp[idx,]);
                }
            }#n
        }
        if (!is.null(mdfr)) mdfr$case<-"tcsam02";
    }

    if (!is.null(mdfr)) mdfr<-rCompTCMs::getMDFR.CanonicalFormat(mdfr);
    if (verbose) cat("--Finished getMDFR.Fits.MaturityData().\n");
    return(mdfr);
}
