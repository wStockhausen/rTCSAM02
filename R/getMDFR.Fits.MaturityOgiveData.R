#'
#'@title Get model fits to maturity ogive data
#'
#'@description Function to get model fits to maturity ogive data.
#'
#'@param objs - single model report list object, or named list of them
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return dataframe (or NULL)
#'
#'@details Returned dataframe is in canonical format
#'
#'@importFrom dplyr bind_rows
#'
#'@export
#'
getMDFR.Fits.MaturityOgiveData<-function(objs,
                                         verbose=FALSE){
    if (verbose) cat("--Starting getMDFR.Fits.MaturityOgiveData().\n");
    options(stringsAsFactors=FALSE);

    mdfr<-NULL;
    if (class(objs)[1]=='list'){
        #repObjs should be a list of tcsam02 resLst objects
        for (nm in names(objs)){
            mdfrp<-getMDFR.Fits.MaturityOgiveData(objs[[nm]],
                                          verbose=verbose);
            if (!is.null(mdfrp)) mdfrp$case<-nm;
            mdfr<-rbind(mdfr,mdfrp);
        }
    } else if (inherits(objs,'tcsam02.resLst')){
        #objs is a single tcsam02 resLst object
        md<-objs$rep$model.fits$maturityogivedata;
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
                                      category=ctg,type="n",
                                      y=mdn$y,z=mdn$z,val=mdn$n);
                    dfrp4<-data.frame(fleet=f,x=x,m="",s="new shell",
                                      category=ctg,type="nlls",
                                      y=mdn$y,z=mdn$z,val=mdn$nlls);
                    dfrp5<-data.frame(fleet=f,x=x,m="",s="new shell",
                                      category=ctg,type="zscores",
                                      y=mdn$y,z=mdn$z,val=mdn$zscrs);
                    mdfrp<-dplyr::bind_rows(dfrp1,dfrp2,dfrp3,dfrp4,dfrp5);
                    idx<-order(mdfrp$type,mdfrp$y,mdfrp$z,mdfrp$val);
                    mdfr<-dplyr::bind_rows(mdfr,mdfrp[idx,]);
                }
            }#n
        } else {
            message("No maturity ogive data was fit.")
        }
        if (!is.null(mdfr)) mdfr$case<-"tcsam02";
    }

    if (!is.null(mdfr)) mdfr<-getMDFR.CanonicalFormat(mdfr);
    if (verbose) cat("--Finished getMDFR.Fits.MaturityOgiveData().\n");
    return(mdfr);
}
