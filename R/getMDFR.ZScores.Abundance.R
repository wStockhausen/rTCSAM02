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
#'@details Uses [getMDFR.ZScoresForABData()].
#'Returned dataframe is in canonical format.
#'
#'@import dplyr
#'@import magrittr
#'
#'@md
#'
#'@export
#'
getMDFR.ZScores.Abundance<-function(objs,
                                    fleet.type=c('survey','fishery'),
                                    catch.type=c('index','retained','discarded','total'),
                                    verbose=FALSE){
    if (verbose) message("--Starting rTCSAM02::getMDFR.ZScores.Abundance().\n");
    options(stringsAsFactors=FALSE);

    fleet.type<-fleet.type[1];
    catch.type<-catch.type[1];
    data.type<-"abundance";

    mdfr<-NULL;
    if (inherits(objs,'tcsam02.resLst')){
        #objs is a tcsam02 resLst object
        if (verbose) message("\tStarting at 1")
        mdfrp<-getMDFR.ZScores.Abundance(objs$rep,
                                         fleet.type=fleet.type,
                                         catch.type=catch.type,
                                         verbose=verbose);
        if (!is.null(mdfrp)){
            mdfrp$case<-'tcsam02';
            mdfr<-rbind(mdfr,mdfrp);
        }
    } else if (inherits(objs,'tcsam02.rep')){
        #objs is a single tcsam02 model report object
      if (verbose) message("\tStarting at 2")
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
                        if (verbose) message("---Getting '",ctNm,"' for ",fltNm,"\n",sep='');
                        mdfrp<-NULL;
                        if ((data.type=="abundance")&&!is.null(ct$abundance)){
                            if (verbose) message("---Getting abundance zscores\n")
                            mdfrp<-rTCSAM02::getMDFR.ZScoresForABData(ct$abundance$fits,verbose=verbose);
                        }
                        if (!is.null(mdfrp)){
                            if (verbose) message("--created dataframe w/ ",nrow(mdfrp),"rows\n")
                            mdfrp$process<-fleet.type;
                            mdfrp$fleet<-fleet;
                            mdfrp$category<-catch.type;
                            #--need to keep only var=="z-score" type
                            mdfr<-rbind(mdfr,mdfrp %>% dplyr::filter(var=='z-score'));
                            mdfrp$type<-mdfrp$var;
                        }
                    } else {
                        if (verbose) message(ctNm,"not found for",fltNm,"\n");
                    }
                }##ctNms
            }
        }##fltNms
        if (!is.null(mdfr)) mdfr$case<-"tcsam02";
    } else if (inherits(objs,'list')){
        #objs should be a list of tcsam02.resLst objects
        if (verbose) message("\tStarting at 3")
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

    if (!is.null(mdfr)) {
      mdfr<-getMDFR.CanonicalFormat(mdfr);
    }

    if (verbose) message("--Finished rTCSAM02::getMDFR.ZScores.Abundance().\n");
    return(mdfr);
}
