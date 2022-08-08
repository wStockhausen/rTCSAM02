#'
#'@title Get all scores related to model fits to abundance time series for fleet data components
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
#'@details Uses [getMDFR.AllScores.Abundance1()] to get scores via [getMDFR.AllScoresForABData()].
#'Returned dataframe is in canonical format.
#'
#' Column \code{type} indicates the type
#' of value, which is one of "z-score","observed","predicted","sdobs","xcv","stdv", and "useFlg".
#'
#'@import dplyr
#'@import magrittr
#'
#'@md
#'
#'@export
#'
getMDFR.AllScores.Abundance<-function(objs,
                                    fleet.type=c('survey','fishery'),
                                    catch.type=c('index','retained','discarded','total'),
                                    verbose=FALSE){
    if (verbose) message("--Starting rTCSAM02::getMDFR.AllScores.Abundance().\n");
    options(stringsAsFactors=FALSE);

    fleet.type<-fleet.type[1];
    catch.type<-catch.type[1];

    mdfr<-NULL;
    if (inherits(objs,'tcsam02.resLst')){
        #objs is a tcsam02 resLst object
        mdfrp<-getMDFR.AllScores.Abundance1(objs$rep,
                                          fleet.type=fleet.type,
                                          catch.type=catch.type,
                                          verbose=verbose);
        if (!is.null(mdfrp)){
            mdfrp$case<-'tcsam02';
            mdfr<-rbind(mdfr,mdfrp);
        }
    } else if (inherits(objs,'tcsam02.rep')){
        mdfr<-getMDFR.AllScores.Abundance1(objs,
                                           fleet.type,
                                           catch.type,
                                           verbose);
    } else if (inherits(objs,'list')){
        #objs should be a list of tcsam02.resLst objects
        for (nm in names(objs)){
            mdfrp<-getMDFR.AllScores.Abundance(objs[[nm]],
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

    if (verbose) message("--Finished rTCSAM02::getMDFR.AllScores.Abundance().\n");
    return(mdfr);
}

#'
#'@title Get all scores related to model fits to abundance time series for fleet data components
#'
#'@description Function to get all scores related to model fits to abundance time series for fleet data components.
#'
#'@param rep - single model report list object
#'@param fleet.type - fleet type ('fishery' or 'survey')
#'@param catch.type - catch type ('index','retained','discarded',or 'total')
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return dataframe (not in canonical format)
#'
#'@details Uses [getMDFR.ZScoresForABData()].
#'Returned dataframe is in canonical format.
#'
#' This function is not exported. Use [getMDFR.AllScores.Abundance()].
#'
#' Column \code{type} indicates the type
#' of value, which is one of "z-score","observed","predicted","sdobs","xcv","stdv", and "useFlg".
#'
#'@import dplyr
#'@import magrittr
#'
#'@md
#'
getMDFR.AllScores.Abundance1<-function(rep,
                                       fleet.type,
                                       catch.type,
                                       verbose){
  if (verbose) message("finished rTCSAM02::getMDFR.AllScores.Abundance1")
  #rep is a single tcsam02 model report object
  if (fleet.type=='fishery'){
      flts<-rep$model.fits$fisheries;
      fltNms<-names(flts);
  } else if (fleet.type=='survey'){
      flts<-rep$model.fits$surveys;
      fltNms<-names(flts);
  } else {
      ##throw error
  }
  mdfr<-NULL;
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
                  if (!is.null(ct$abundance)){
                      if (verbose) message("---Getting abundance scores\n")
                      mdfrp<-getMDFR.AllScoresForABData(ct$abundance$fits,verbose=verbose);
                      if (!is.null(mdfrp)){
                          if (verbose) message("--created dataframe w/",nrow(mdfrp),"rows\n")
                          mdfrp$case<-"tcsam02";
                          mdfrp$process<-fleet.type;
                          mdfrp$fleet<-fleet;
                          mdfrp$category<-catch.type;
                          mdfr<-rbind(mdfr,mdfrp);
                      }
                  }
              } else {
                  if (verbose) message(ctNm,"not found for",fltNm,"\n");
              }
          }##ctNms
      }##--fltNm!=''
  }##fltNms

  if (!is.null(mdfr)) mdfr %<>% dplyr::rename(type=var);

  if (!is.null(mdfr)) mdfr<-getMDFR.CanonicalFormat(mdfr);

  if (verbose) message("finished rTCSAM02::getMDFR.AllScores.Abundance1")
  return(mdfr);
}

