#'
#'@title Get model fits to growth data
#'
#'@description Function to get model fits to growth data.
#'
#'@param objs - single model report list object, or named list of them
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return dataframe
#'
#'@details Returned dataframe is in canonical format
#'
#'@export
#'
getMDFR.Fits.GrowthData<-function(objs,
                                 verbose=FALSE){
    if (verbose) cat("--Starting getMDFR.Fits.GrowthData().\n");
    options(stringsAsFactors=FALSE);

    mdfr<-NULL;
    if (class(objs)[1]=='list'){
        #repObjs should be a list of tcsam02 resLst objects
        for (nm in names(objs)){
            mdfrp<-getMDFR.Fits.GrowthData(objs[[nm]],
                                          verbose=verbose);
            if (!is.null(mdfrp)) mdfrp$case<-nm;
            mdfr<-rbind(mdfr,mdfrp);
        }
    } else if (inherits(objs,'tcsam02.resLst')){
        #objs is a single tcsam02 resLst object
        gd<-objs$rep$model.fits$growthdata;
        ngd<-length(gd)-1;#last element is NULL
        if (ngd>0){
            nms.gd<-names(gd);
            for (n in 1:ngd){
                gdn<-gd[[n]];
                ngdn<-length(gdn)-1;
                if (ngdn>0){
                    nms.gdn<-names(gdn);
                    for (nx in 1:ngdn){
                        x    <-tolower(names(gdn)[nx]);
                        gdnx<-gdn[[nx]];
                        dfrp1<-data.frame(x=x,m="immature",s="new shell",
                                          category=nms.gd[n],type="observed",
                                          y=gdnx$years,z=gdnx$zPre,val=gdnx$zPst);
                        dfrp2<-data.frame(x=x,m="immature",s="new shell",
                                          category=nms.gd[n],type="predicted",
                                          y=gdnx$years,z=gdnx$zPre,val=gdnx$mnZ);
                        dfrp3<-data.frame(x=x,m="immature",s="new shell",
                                          category=nms.gd[n],type="nlls",
                                          y=gdnx$years,z=gdnx$zPre,val=gdnx$nlls);
                        dfrp4<-data.frame(x=x,m="immature",s="new shell",
                                          category=nms.gd[n],type="zscores",
                                          y=gdnx$years,z=gdnx$zPre,val=gdnx$zscrs);
                        mdfrp<-rbind(dfrp1,dfrp2,dfrp3,dfrp4);
                        idx<-order(mdfrp$type,mdfrp$y,mdfrp$z,mdfrp$val);
                        mdfr<-rbind(mdfr,mdfrp[idx,]);
                    }#nx
                }
            }#n
        }
        if (!is.null(mdfr)) mdfr$case<-"tcsam02";
    }

    if (!is.null(mdfr)) mdfr<-rCompTCMs::getMDFR.CanonicalFormat(mdfr);
    if (verbose) cat("--Finished getMDFR.Fits.GrowthData().\n");
    return(mdfr);
}
