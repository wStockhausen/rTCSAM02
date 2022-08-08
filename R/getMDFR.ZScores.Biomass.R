#'
#'@title Get model fits to biomass time series as z-scores for fleet data components
#'
#'@description Function to get model fits to biomass time series as z-scores for fleet data components.
#'
#'@param objs - single model report list object, or named list of them
#'@param fleet.type - fleet type ('fishery' or 'survey')
#'@param catch.type - catch type ('index','retained','discarded',or 'total')
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return dataframe
#'
#'@details Uses [getMDFR.AllScores.Biomass()] but  keeps only z-scores.
#'Returned dataframe is in canonical format.
#'
#'@import dplyr
#'@import magrittr
#'
#'@md
#'
#'@export
#'
getMDFR.ZScores.Biomass<-function(objs,
                                    fleet.type=c('survey','fishery'),
                                    catch.type=c('index','retained','discarded','total'),
                                    verbose=FALSE){
    if (verbose) message("--Starting rTCSAM02::getMDFR.ZScores.Biomass().\n");
    options(stringsAsFactors=FALSE);

    fleet.type<-fleet.type[1];
    catch.type<-catch.type[1];

    mdfr<-NULL;
    if (inherits(objs,'tcsam02.resLst')){
        #objs is a tcsam02 resLst object
        mdfrp<-getMDFR.ZScores.Biomass(objs$rep,
                                         fleet.type=fleet.type,
                                         catch.type=catch.type,
                                         verbose=verbose);
        if (!is.null(mdfrp)){
            mdfrp$case<-'tcsam02';
            mdfr<-rbind(mdfr,mdfrp);
        }
    } else if (inherits(objs,'tcsam02.rep')){
        mdfr<-getMDFR.AllScores.Biomass(objs,
                                        fleet.type,
                                        catch.type,
                                        verbose);
        mdfr %<>% dplyr::filter(type=="z-score");    #--SAVE ONLY **z-scores**
    } else if (inherits(objs,'list')){
        #objs should be a list of tcsam02.resLst objects
        for (nm in names(objs)){
            mdfrp<-getMDFR.ZScores.Biomass(objs[[nm]],
                                            fleet.type=fleet.type,
                                            catch.type=catch.type,
                                            verbose=verbose);
            if (!is.null(mdfrp)) mdfrp$case<-nm;
            mdfr<-rbind(mdfr,mdfrp);
        }
    } else {
        #throw an error
    }

    if (!is.null(mdfr)) mdfr<-getMDFR.CanonicalFormat(mdfr);

    if (verbose) message("--Finished rTCSAM02::getMDFR.ZScores.Biomass().\n");
    return(mdfr);
}

