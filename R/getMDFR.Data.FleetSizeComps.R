#'
#'@title Get fleet size comps by model scenario
#'
#'@description Function to get fleet size comps by model scenario.
#'
#'@param objs - single model report list object, or named list of them
#'@param fleet.type - fleet type ('fishery' or 'survey')
#'@param catch.type - catch type ('index','total','discard', or 'retained')
#'@param ci - confidence intervals for time series observations
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return dataframe in canonical format
#'
#'@details Returned dataframe is in canonical format. "category" will be "n.at.z"
#'to indicate size compositions (to be consistent across the package).
#'
#'@export
#'
getMDFR.Data.FleetSizeComps<-function(objs,
                                       fleet.type=c('survey','fishery'),
                                       catch.type=c('index','total','discard','retained'),
                                       verbose=FALSE){
    fleet.type<-fleet.type[1];
    catch.type<-catch.type[1];
    data.type<-"nAtZ";

    options(stringsAsFactors=FALSE);

    mdfr<-NULL;
    if (class(objs)[1]=='list'){
        #repObjs should be a list of tcsam02 resLst objects
        for (nm in names(objs)){
            mdfrp<-getMDFR.Data.FleetSizeComps(objs[[nm]],
                                               fleet.type=fleet.type,
                                               catch.type=catch.type,
                                               verbose=verbose);
            if (!is.null(mdfrp)) mdfrp$case<-nm;
            mdfr<-rbind(mdfr,mdfrp);
        }
    } else if (inherits(objs,'tcsam02.resLst')){
        if (verbose) {
            cat("--Starting rTCSAM02::getMDFR.Data.FleetSizeComps().\n");
            cat("---Extracting fleet.type = ",fleet.type,", catch.type = ",catch.type,"\n");
        }
        #objs is a single tcsam02 resLst object
        if (fleet.type=='fishery'){
            flts<-objs$rep$data$fisheries;
            fltNms<-names(flts);
        } else if (fleet.type=='survey'){
            flts<-objs$rep$data$surveys;
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
                    ct<-flt[[ctNm]];
                    if (!is.null(ct)){
                        if (verbose) cat("----Getting '",ctNm,"' for ",fltNm,"\n",sep='');
                        cat("names(ct):",paste(names(ct),collapse=" "),"\n");
                        dt<-ct[[data.type]];
                        if (!is.null(dt)){
                            if (verbose){
                                cat("----names(dt):",paste(names(dt),collapse=" "),"\n");
                                cat("----units:",dt$units,"\n");
                                cat("----lltype:",dt$llType,"\n")
                            }
                            dts<-reshape2::melt(dt$data);
                            sss<-reshape2::melt(dt$sample.sizes);
                            dfrp1<-data.frame(x=dts$x,
                                              m=dts$m,
                                              s=dts$s,
                                              y=dts$y,
                                              z=dts$z,
                                              val=dts$value,
                                              lci=NA_real_,
                                              uci=NA_real_,
                                              type="observed",
                                              stringsAsFactors=FALSE);
                            dfrp1$category<-"n.at.z";#--this is correct: category and data.type are slightly different
                            dfrp1$process<-fleet.type;
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


    if (!is.null(mdfr)) mdfr<-getMDFR.CanonicalFormat(mdfr);
    if (verbose) cat("--Finished rTCSAM02::getMDFR.Data.FleetTimeSeries().\n");
    return(mdfr);
}
