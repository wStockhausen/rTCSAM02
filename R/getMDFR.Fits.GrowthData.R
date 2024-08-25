#'
#'@title Get model fits to growth data as a dataframe
#'
#'@description Function to get model fits to growth data as a dataframe.
#'
#'@param objs - a single tcsam02 rep object, a single tcsam02 resLst object, or a named list of such
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return dataframe (or NULL)
#'
#'@details Returned dataframe is in canonical format.
#'
#'@export
#'
getMDFR.Fits.GrowthData<-function(objs,
                                 verbose=FALSE){
    if (verbose) cat("--Starting getMDFR.Fits.GrowthData().\n");
    options(stringsAsFactors=FALSE);

    mdfr<-NULL;
    if (class(objs)[1]=='list'){
        #objs should be a named list of tcsam02 resLst or rep objects
        if (verbose) cat("----objs is a list.\n");
        for (nm in names(objs)){
            mdfrp<-getMDFR.Fits.GrowthData(objs[[nm]],
                                          verbose=verbose);
            if (!is.null(mdfrp)) mdfrp$case<-nm;
            mdfr<-rbind(mdfr,mdfrp);
            return(mdfr);
        }
    } else if (inherits(objs,'tcsam02.resLst')){
        #objs is a single tcsam02 resLst object
        if (verbose) cat("------objs is a tcsam02.resLst object.\n");
        mdfr<-getMDFR.Fits.GrowthData(objs$rep,
                                      verbose=verbose);
        return(mdfr);
    } else if (!inherits(objs,'tcsam02.rep')){
        cat("--!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!--\n")
        cat("Error in rTCSAM02::getMDFR.Fits.GrowthData().\n")
        cat("Input object not reducible to a tcsam02.rep object!\n")
        cat("Classes = ",class(objs),"\n");
        cat("Returning NULL.\n")
        cat("--!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!--\n")
        return(NULL);
    }

    #objs is a single tcsam02 rep object
    if (verbose) cat("--------objs is a tcsam02.rep object.\n");
    gd<-objs$model.fits$growthdata;
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
                                      y=gdnx$years,z=gdnx$zPre,val=gdnx$zPst) |>
                            dplyr::mutate(pc=dplyr::row_number());
                    dfrp2<-data.frame(x=x,m="immature",s="new shell",
                                      category=nms.gd[n],type="predicted",
                                      y=gdnx$years,z=gdnx$zPre,val=gdnx$mnZ) |>
                            dplyr::mutate(pc=dplyr::row_number());
                    dfrp3<-data.frame(x=x,m="immature",s="new shell",
                                      category=nms.gd[n],type="nlls",
                                      y=gdnx$years,z=gdnx$zPre,val=gdnx$nlls) |>
                            dplyr::mutate(pc=dplyr::row_number());
                    dfrp4<-data.frame(x=x,m="immature",s="new shell",
                                      category=nms.gd[n],type="zscores",
                                      y=gdnx$years,z=gdnx$zPre,val=gdnx$zscrs) |>
                            dplyr::mutate(pc=dplyr::row_number());
                    dfrp5<-data.frame(x=x,m="immature",s="new shell",
                                      category=nms.gd[n],type="alpha",
                                      y=gdnx$years,z=gdnx$zPre,val=gdnx$alpha) |>
                            dplyr::mutate(pc=dplyr::row_number());
                    dfrp6<-data.frame(x=x,m="immature",s="new shell",
                                      category=nms.gd[n],type="ibeta",
                                      y=gdnx$years,z=gdnx$zPre,val=gdnx$ibeta) |>
                            dplyr::mutate(pc=dplyr::row_number());
                    mdfrp<-rbind(dfrp1,dfrp2,dfrp3,dfrp4,dfrp5,dfrp6);
                    idx<-order(mdfrp$type,mdfrp$y,mdfrp$z,mdfrp$val);
                    mdfr<-rbind(mdfr,mdfrp[idx,]);
                }#nx
            }
        }#n
    }
    if (!is.null(mdfr)) {
        mdfr$case<-"tcsam02";
        mdfr<-getMDFR.CanonicalFormat(mdfr);
    }
    if (verbose) cat("--Finished getMDFR.Fits.GrowthData().\n");
    return(mdfr);
}
