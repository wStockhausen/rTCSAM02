#'
#'@title Get model fits to abundance time series as z-scores for fleet data components
#'
#'@description Function to get model fits to abundance time series as z-scores for fleet data components.
#'
#'@param objs - single model report list object, or named list of them
#'@param fleet.type - fleet type ('fishery' or 'survey')
#'@param catch.type - catch type ('index','retained','discarded',or 'total')
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return dataframe
#'
#'@details Uses \code{getMDFR.ZScoresForABData()}.
#'Returned dataframe is in canonical format.
#'
#'@export
#'
getMDFR.ZScores.Abundance<-function(objs,
                                    fleet.type=c('survey','fishery'),
                                    catch.type=c('index','retained','discarded','total'),
                                    verbose=FALSE){
    if (verbose) cat("--Starting rTCSAM02::getMDFR.ZScores.Abundance().\n");
    options(stringsAsFactors=FALSE);

    fleet.type<-fleet.type[1];
    catch.type<-catch.type[1];
    data.type<-"abundance";

    mdfr<-NULL;
    if (inherits(objs,'tcsam02.resLst')){
        #objs is a tcsam02 resLst object
        mdfrp<-getMDFR.ZScores.Abundance(objs$rep,
                                         fleet.type=fleet.type,
                                         catch.type=catch.type,
                                         verbose=verbose);
        if (!is.null(mdfrp)){
            mdfrp$case<-'tcsam02';
            mdfr<-rbind(mdfr,mdfrp);
        }
    } else if (inherits(objs,'tcsam02.rep')){
        #objs is a single tcsam2015 model report object
        if (fleet.type=='fishery'){
            flts<-objs$model.fits$fisheries;
            fltNms<-names(flts);
        } else if (fleet.type=='survey'){
            flts<-objs$model.fits$surveys;
            fltNms<-names(flts);
        } else {
            ##throw error
        }
        ctNms<-catch.type;
        for (fltNm in fltNms){
            if (fltNm!=''){
                fleet<-gsub("_"," ",fltNm,fixed=TRUE);
                flt<-flts[[fltNm]];
                for (ctNm in ctNms){
                    #catch.type<-gsub("."," ",ctNm,fixed=TRUE);
                    ctNm<-paste0(ctNm,".catch");
                    ct<-flt[[ctNm]];
                    if (!is.null(ct)){
                        if (verbose) cat("---Getting '",ctNm,"' for ",fltNm,"\n",sep='');
                        mdfrp<-NULL;
                        if ((data.type=="abundance")&&!is.null(ct$abundance)){
                            if (verbose) cat("---Getting abundance zscores\n")
                            mdfrp<-getMDFR.ZScoresForABData(ct$abundance$fits,verbose=verbose);
                        }
                        if (!is.null(mdfrp)){
                            if (verbose) cat("--created dataframe w/",nrow(mdfrp),"rows\n")
                            mdfrp$process<-fleet.type;
                            mdfrp$fleet<-fleet;
                            mdfrp$category<-catch.type;
                            mdfr<-rbind(mdfr,mdfrp);
                        }
                    } else {
                        if (verbose) cat(ctNm,"not found for",fltNm,"\n");
                    }
                }##ctNms
            }
        }##fltNms
        if (!is.null(mdfr)) mdfr$case<-"tcsam02";
    } else if (inherits(objs,'list')){
        #objs should be a list of tcsam02.resLst objects
        for (nm in names(objs)){
            mdfrp<-getMDFR.ZScores.Abundance(objs[[nm]],
                                            fleet.type=fleet.type,
                                            catch.type=catch.type,
                                            verbose=verbose);
            if (!is.null(mdfrp)) mdfrp$case<-nm;
            mdfr<-rbind(mdfr,mdfrp);
        }
    } else {
        #throw an error
    }

    if (!is.null(mdfr)) mdfr<-rCompTCMs::getMDFR.CanonicalFormat(mdfr);

    if (verbose) cat("--Finished rTCSAM02::getMDFR.ZScores.Abundance().\n");
    return(mdfr);
}
