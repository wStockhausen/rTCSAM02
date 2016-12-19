#'
#'@title Get model fits to biomass for fleet data components
#'
#'@description Function to get model fits to biomass data for fleet data components.
#'
#'@param objs - single model report list object, or named list of them
#'@param fleet.type - fleet type ('fishery' or 'survey')
#'@param data.type - data type ('abundance', 'biomass', or 'n.at.z')
#'@param catch.type - cath type ('index','total','discard', or 'retained')
#'@param ci - confidence intervals for observations
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return dataframe
#'
#'@details Uses \code{getMDFR.FitsForABData()} and \code{getMDFR.FitsForSizeComps()}.
#' Returned dataframe is in canonical format
#'
#'@export
#'
getMDFR.Fits.FleetData<-function(objs,
                                 fleet.type=c('survey','fishery'),
                                 data.type=c('abundance', 'biomass', 'n.at.z'),
                                 catch.type=c('index','total','discard','retained'),
                                 ci=0.80,
                                 verbose=FALSE){
    if (verbose) {
        cat("--Starting rTCSAM02::getMDFR.Fits.FleetData().\n");
        cat("Extracting fleet.type = ",fleet.type,", data.type = ",data.type,", catch.type = ",catch.type,"\n");
    }
    options(stringsAsFactors=FALSE);

    fleet.type<-fleet.type[1];
    data.type<-data.type[1];
    catch.type<-catch.type[1];

    mdfr<-NULL;
    if (class(objs)[1]=='list'){
        #repObjs should be a list of tcsam02 resLst objects
        for (nm in names(objs)){
            mdfrp<-getMDFR.Fits.FleetData(objs[[nm]],
                                          fleet.type=fleet.type,
                                          data.type=data.type,
                                          catch.type=catch.type,
                                          ci=ci,
                                          verbose=verbose);
            if (!is.null(mdfrp)) mdfrp$case<-nm;
            mdfr<-rbind(mdfr,mdfrp);
        }
    } else if (inherits(objs,'tcsam02.resLst')){
        #objs is a single tcsam02 resLst object
        if (fleet.type=='fishery'){
            flts<-objs$rep$model.fits$fisheries;
            fltNms<-names(flts);
        } else if (fleet.type=='survey'){
            flts<-objs$rep$model.fits$surveys;
            fltNms<-names(flts);
        } else {
            ##throw error
        }
        ctNms<-paste0(catch.type,".catch");
        for (fltNm in fltNms){
            if (fltNm!=''){
                fleet<-gsub("_"," ",fltNm,fixed=TRUE);
                flt<-flts[[fltNm]];
                for (ctNm in ctNms){
                    #catch.type<-gsub("."," ",ctNm,fixed=TRUE);
                    ct<-flt[[ctNm]];
                    if (!is.null(ct)){
                        if (verbose) cat("---Getting '",ctNm,"' for ",fltNm,"\n",sep='');
                        mdfrp<-NULL;
                        if ((data.type=="abundance")&&!is.null(ct$abundance)){
                            if (verbose) cat("---Getting abundance fits\n")
                            mdfrp<-getMDFR.FitsForABData(ct$abundance$fits,ci=ci,verbose=verbose);
                            if (!is.null(mdfrp)) mdfrp$category<-"abundance";
                        } else
                        if ((data.type=="biomass")&&!is.null(ct$biomass)){
                            if (verbose) cat("---Getting biomass fits\n")
                            mdfrp<-getMDFR.FitsForABData(ct$biomass$fits,ci=ci,verbose=verbose);
                            if (!is.null(mdfrp)) mdfrp$category<-"biomass";
                        } else
                        if ((data.type=="n.at.z")&&!is.null(ct$n.at.z)){
                            if (verbose) cat("---Getting n.at.z zscores\n")
                            mdfrp<-getMDFR.FitsForSizeComps(ct$n.at.z,objs$rep$mc,verbose=verbose);
                            if (!is.null(mdfrp)) mdfrp$category<-"n.at.z";
                        } else {
                            if (verbose) cat("--data.type =",data.type,"not found!\n");
                        }
                        if (!is.null(mdfrp)){
                            if (verbose) cat("---created dataframe w/",nrow(mdfrp),"rows\n")
                            mdfrp$process<-fleet.type;
                            mdfrp$fleet<-fleet;
                            mdfrp$type<-mdfrp$var;
                            mdfr<-rbind(mdfr,mdfrp);
                        }
                    }
                }##ctNms
            }
        }##fltNms
        if (!is.null(mdfr)) mdfr$case<-"tcsam02";
    }

    if (!is.null(mdfr)) mdfr<-rCompTCMs::getMDFR.CanonicalFormat(mdfr);
    if (verbose) cat("--Finished getMDFR.Fits.FleetData().\n");
    return(mdfr);
}
