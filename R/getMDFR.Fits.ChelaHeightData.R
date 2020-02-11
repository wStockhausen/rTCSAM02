#'
#'@title Get model fits to chela height data
#'
#'@description Function to get model fits to chela height data.
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
getMDFR.Fits.ChelaHeightData<-function(objs,
                                 verbose=FALSE){
    if (verbose) cat("--Starting getMDFR.Fits.ChelaHeightData().\n");
    options(stringsAsFactors=FALSE);

    mdfr<-NULL;
    if (class(objs)[1]=='list'){
        #repObjs should be a list of tcsam02 resLst objects
        for (nm in names(objs)){
            mdfrp<-getMDFR.Fits.ChelaHeightData(objs[[nm]],
                                          verbose=verbose);
            if (!is.null(mdfrp)) mdfrp$case<-nm;
            mdfr<-rbind(mdfr,mdfrp);
        }
    } else if (inherits(objs,'tcsam02.resLst')){
        #objs is a single tcsam02 resLst object
        md<-objs$rep$model.fits$chelaheightdata;
        nmd<-length(md)-1;#last element is NULL
        if (nmd>0){
            nms.md<-names(md);
            for (n in 1:nmd){
                mdn<-md[[n]];
                nmdn<-length(mdn)-1;
                if (nmdn>0){
                    ctg<-gsub("_"," ",nms.md[n],fixed=TRUE);
                    f<-mdn$fleet;
                    x<-tolower(mdn$sex);
                    dfrp1<-data.frame(fleet=f,x=x,m="",s="new shell",
                                      category=ctg,type="observed",
                                      y=mdn$y,z=mdn$z,val=mdn$obsPM);
                    dfrp2<-data.frame(fleet=f,x=x,m="",s="new shell",
                                      category=ctg,type="predicted",
                                      y=mdn$y,z=mdn$z,val=mdn$modPM);
                    dfrp3<-data.frame(fleet=f,x=x,m="",s="new shell",
                                      category=ctg,type="nlls",
                                      y=mdn$y,z=mdn$z,val=mdn$nlls);
                    dfrp4<-data.frame(fleet=f,x=x,m="",s="new shell",
                                      category=ctg,type="zscores",
                                      y=mdn$y,z=mdn$z,val=mdn$zscrs);
                    mdfrp<-rbind(dfrp1,dfrp2,dfrp3,dfrp4);
                    idx<-order(mdfrp$type,mdfrp$y,mdfrp$z,mdfrp$val);
                    mdfr<-rbind(mdfr,mdfrp[idx,]);
                }
            }#n
        } else {
            message("No chela height data was fit.")
        }
        if (!is.null(mdfr)) mdfr$case<-"tcsam02";
    }

    if (!is.null(mdfr)) mdfr<-getMDFR.CanonicalFormat(mdfr);
    if (verbose) cat("--Finished getMDFR.Fits.ChelaHeightData().\n");
    return(mdfr);
}
