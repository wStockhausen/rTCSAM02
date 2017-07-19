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
#'@return dataframe
#'
#'@details Returned dataframe is in canonical format
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
            cat("--Starting rTCSAM02::getMDFR.Data.FleetTimeSeries().\n");
            cat("---Extracting fleet.type = ",fleet.type,", data.type = ",data.type,", catch.type = ",catch.type,"\n");
            cat("---ci: ",paste(ci,collapse=" "),"\n")
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
                            cvs<-reshape2::melt(dt$cvs);
                            pdfType<-dt$llType;
                            obs<-dts$value;
                            if (tolower(pdfType)=='normal'){
                                #normal, sdv on arithmetic scale
                                if (verbose) cat('----using err type = normal\n')
                                sdv<-cvs$value*dts$value;
                                lci<-qnorm(ci[1],mean=obs,sd=sdv);
                                uci<-qnorm(ci[2],mean=obs,sd=sdv);
                            } else if (tolower(pdfType)=='lognormal'){
                                #lognormal, sdv on ln-scale
                                if (verbose) cat('----using err type = lognormal\n')
                                sdv<-sqrt(log(1.0+cvs$value^2));
                                lci<-qlnorm(ci[1],meanlog=log(obs),sdlog=sdv);
                                uci<-qlnorm(ci[2],meanlog=log(obs),sdlog=sdv);
                            } else if (tolower(pdfType)=='norm2'){
                                #normal, sdv on arithmetic scale
                                if (verbose) cat('----using err type = normal, but fit uses norm2\n')
                                lci<-qnorm(ci[1],mean=obs,sd=sqrt(0.5));
                                uci<-qnorm(ci[2],mean=obs,sd=sqrt(0.5));
                            } else if (tolower(pdfType)=='none'){
                                if (verbose) cat('---using err type = none\n')
                                lci<-NA;
                                uci<-NA;
                            } else {
                                cat('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n')
                                cat('Error in getMDFR.Data.FleetData.\n')
                                cat("pdfType '",pdfType,"' not recognized!!\n")
                                cat("Exiting function.\n")
                                cat('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n')
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
                        cat("No '",ctNm,"' for ",fltNm,"\n",sep='')
                    }
                }##ctNms
            }
        }##fltNms
        if (!is.null(mdfr)) mdfr$case<-"tcsam02";
    }


    if (!is.null(mdfr)) mdfr<-rCompTCMs::getMDFR.CanonicalFormat(mdfr);
    if (verbose) cat("--Finished rTCSAM02::getMDFR.Data.FleetTimeSeries().\n");
    return(mdfr);
}
