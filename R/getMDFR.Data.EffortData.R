#'
#'@title Get model fits to biomass for fleet data components
#'
#'@description Function to get effort data for multiple model scenarios
#'
#'@param objs - single model resLst object, or named list of them
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return dataframe
#'
#'@details Returned dataframe is in canonical format
#'
#'@export
#'
getMDFR.Data.EffortData<-function(objs,
                                 verbose=FALSE){
    options(stringsAsFactors=FALSE);

    mdfr<-NULL;
    if (class(objs)[1]=='list'){
        #repObjs should be a list of tcsam02 resLst objects
        for (nm in names(objs)){
            mdfrp<-getMDFR.Data.EffortData(objs[[nm]],
                                          verbose=verbose);
            if (!is.null(mdfrp)) mdfrp$case<-nm;
            mdfr<-rbind(mdfr,mdfrp);
        }
    } else if (inherits(objs,'tcsam02.resLst')){
        if (verbose) cat("--Starting rTCSAM02::getMDFR.Data.EffortData().\n");
        #objs is a single tcsam02 resLst object
        flts<-objs$rep$data$fisheries;
        fltNms<-names(flts);

        for (fltNm in fltNms){
            if (fltNm!=''){
                fleet<-gsub("_"," ",fltNm,fixed=TRUE);
                flt<-flts[[fltNm]];
                for (ctNm in "effort"){
                    ct<-flt[[ctNm]];
                    if (!is.null(ct)){
                        if (verbose) cat("----Getting '",ctNm,"' for ",fltNm,"\n",sep='');
                        cat("names(ct):",paste(names(ct),collapse=" "),"\n");
                        dt<-ct[["data"]];
                        if (!is.null(dt)){
                            if (verbose) cat("----names(dt):",paste(names(dt),collapse=" "),"\n");
                            dts<-reshape2::melt(dt);
                            dfrp1<-NULL;
                            dfrp1<-data.frame(x=NA,m=NA,s=NA,z=NA,
                                              y=dts$year,
                                              val=dts$value,lci=NA,uci=NA,type="observed");
                            dfrp1$category<-"effort";
                            dfrp1$process<-"fishery";
                            dfrp1$fleet<-fleet;
                            if(!is.null(dfrp1)) mdfr<-rbind(mdfr,dfrp1);
                        }
                    } else {
                        cat("No '",ctNm,"' for ",fltNm,"\n",sep='')
                    }
                }##ctNms
            }
        }##fltNms
        if (!is.null(mdfr)) mdfr$case<-"tcsam02";
    }

    if (!is.null(mdfr)) mdfr<-rCompTCMs::getMDFR.CanonicalFormat(mdfr);
    if (verbose) cat("--Finished getMDFR.Data.FleetData().\n");
    return(mdfr);
}
