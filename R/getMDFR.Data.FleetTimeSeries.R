#'
#'@title Get fleet data components by model scenario
#'
#'@description Function to get fleet data components by model scenario
#'
#'@param objs - single model report list object, or named list of them
#'@param fleet.type - fleet type ('fishery' or 'survey')
#'@param data.type - data type ('abundance' or 'biomass')
#'@param catch.type - cath type ('index','total','discard', or 'retained')
#'@param ci - confidence intervals for time series observations
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return dataframe in canonical format (see [getMDFR.CanonicalFormat])
#'
#'@details Returned dataframe is in canonical format
#'
#'@md
#'
#'@export
#'
getMDFR.Data.FleetTimeSeries<-function(objs,
                                       fleet.type=c('survey','fishery'),
                                       data.type=c('abundance', 'biomass'),
                                       catch.type=c('index','total','discard','retained'),
                                       ci=0.80,
                                       verbose=FALSE){
    fleet.type<-fleet.type[1];
    data.type<-data.type[1];
    catch.type<-catch.type[1];

    options(stringsAsFactors=FALSE);

    mdfr<-NULL;
    if (class(objs)[1]=='list'){
        #repObjs should be a list of tcsam02 resLst objects
        for (nm in names(objs)){
            mdfrp<-getMDFR.Data.FleetTimeSeries(objs[[nm]],
                                                fleet.type=fleet.type,
                                                data.type=data.type,
                                                catch.type=catch.type,
                                                ci=ci,
                                                verbose=verbose);
            if (!is.null(mdfrp)) mdfrp$case<-nm;
            mdfr<-rbind(mdfr,mdfrp);
        }
    } else if (inherits(objs,'tcsam02.resLst')){
        ci<-c((1-ci)/2,1-(1-ci)/2);
        if (verbose) {
            message("--Starting rTCSAM02::getMDFR.Data.FleetTimeSeries().\n");
            message("---Extracting fleet.type = ",fleet.type,", data.type = ",data.type,", catch.type = ",catch.type,"\n");
            message("---ci: ",paste(ci,collapse=" "),"\n")
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
                        if (verbose) {
                          message("----Getting '",ctNm,"' for ",fltNm,"\n",sep='');
                          message("names(ct):",paste(names(ct),collapse=" "),"\n");
                        }
                        dt<-ct[[data.type]];
                        if (!is.null(dt)){
                            if (verbose){
                                message("----names(dt):",paste(names(dt),collapse=" "),"\n");
                                message("----units:",dt$units,"\n");
                                message("----lltype:",dt$llType,"\n")
                            }
                            dts<-reshape2::melt(dt$data);
                            cvs<-reshape2::melt(dt$cvs);
                            pdfType<-dt$llType;
                            obs<-dts$value;
                            if (tolower(pdfType)=='normal'){
                                #normal, sdv on arithmetic scale
                                if (verbose) message('----using err type = normal\n')
                                sdv<-cvs$value*dts$value;
                                lci<-stats::qnorm(ci[1],mean=obs,sd=sdv);
                                uci<-stats::qnorm(ci[2],mean=obs,sd=sdv);
                            } else if (tolower(pdfType)=='lognormal'){
                                #lognormal, sdv on ln-scale
                                if (verbose) message('----using err type = lognormal\n')
                                sdv<-sqrt(log(1.0+cvs$value^2));
                                lci<-stats::qlnorm(ci[1],meanlog=log(obs),sdlog=sdv);
                                uci<-stats::qlnorm(ci[2],meanlog=log(obs),sdlog=sdv);
                            } else if (tolower(pdfType)=='norm2'){
                                #normal, sdv on arithmetic scale
                                if (verbose) message('----using err type = normal, but fit uses norm2\n')
                                lci<-stats::qnorm(ci[1],mean=obs,sd=sqrt(0.5));
                                uci<-stats::qnorm(ci[2],mean=obs,sd=sqrt(0.5));
                            } else if (tolower(pdfType)=='none'){
                                if (verbose) message('---using err type = none\n')
                                lci<-NA;
                                uci<-NA;
                            } else {
                                message('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n')
                                message('Error in getMDFR.Data.FleetData.\n')
                                message("pdfType '",pdfType,"' not recognized!!\n")
                                message("Exiting function.\n")
                                message('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n')
                                return(NULL)
                            }
                            dfrp1<-data.frame(x=dts$x,m=dts$m,s=dts$s,
                                              y=dts$y,
                                              val=obs,lci=lci,uci=uci,type="observed");
                            dfrp1$category<-data.type;
                            dfrp1$process<-fleet.type;
                            dfrp1$fleet<-fleet;
                            if(!is.null(dfrp1)) mdfr<-rbind(mdfr,dfrp1);
                        }
                    } else {
                        if (verbose) message("No '",ctNm,"' for ",fltNm,"\n",sep='')
                    }
                }##ctNms
            }
        }##fltNms
        if (!is.null(mdfr)) mdfr$case<-"tcsam02";
    }


    if (!is.null(mdfr)) mdfr<-getMDFR.CanonicalFormat(mdfr);
    if (verbose) message("--Finished rTCSAM02::getMDFR.Data.FleetTimeSeries().\n");
    return(mdfr);
}
