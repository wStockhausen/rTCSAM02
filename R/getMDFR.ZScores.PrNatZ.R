#'
#'@title Get model fits to size frequencies as z-scores for fleet data components
#'
#'@description Function to get model fits to size frequencies as z-scores for fleet data components.
#'
#'@param objs - single model report list object, or named list of them
#'@param fleet.type - fleet type ('fishery' or 'survey')
#'@param catch.type - catch type ('index','retained','discarded',or 'total')
#'@param residuals.type - residual type ('pearsons' or 'nlls')
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return dataframe
#'
#'@details Uses \code{getMDFR.ZScores.PrNatZ1()}.
#' Returned dataframe is in canonical form.
#'
#'@export
#'
getMDFR.ZScores.PrNatZ<-function(objs,
                                 fleet.type=c('survey','fishery'),
                                 catch.type=c('index','retained','discarded','total'),
                                 residuals.type=c('pearsons','nlls'),
                                 verbose=FALSE){
    if (verbose) cat("--Starting rTCSAM02::getMDFR.ZScores.PrNatZ.\n");
    options(stringsAsFactors=FALSE);

    fleet.type<-fleet.type[1];
    catch.type<-catch.type[1];
    residuals.type<-residuals.type[1];
    data.type<-"n.at.z";

    mdfr<-NULL;
    if (inherits(objs,'tcsam02.resLst')){
        #objs is a tcsam02 resLst object
        mdfrp<-getMDFR.ZScores.PrNatZ(objs$rep,
                                      fleet.type=fleet.type,
                                      catch.type=catch.type,
                                      residuals.type=residuals.type,
                                      verbose=verbose);
        if (!is.null(mdfrp)) mdfr<-rbind(mdfr,mdfrp);
    } else if (inherits(objs,'tcsam02.rep')){
        #objs is a single tcsam02 model report object
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
                        if ((data.type=="n.at.z")&&!is.null(ct$n.at.z)){
                            if (verbose) cat("---Getting n.at.z zscores\n")
                            mdfrp<-getMDFR.ZScores.PrNatZ1(ct$n.at.z,
                                                           objs$mc,
                                                           type=residuals.type,
                                                           verbose=verbose);
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
        #objs is a list of tcsam02.resLst objects
        nl<-length(objs);
        nms<-names(objs);
        for (l in 1:nl){
            obj1<-objs[[l]];
            mdfrp<-getMDFR.ZScores.PrNatZ(obj1,
                                          fleet.type=fleet.type,
                                          catch.type=catch.type,
                                          residuals.type=residuals.type,
                                          verbose=verbose);
            if (!is.null(mdfrp)){
                if (!is.null(nms[l])) mdfrp$case<-nms[l];
                mdfr<-rbind(mdfr,mdfrp);
            }
        }
    } else {
        #throw n error
    }

    if (!is.null(mdfr)) mdfr<-getMDFR.CanonicalFormat(mdfr);

    if (verbose) cat("--Finished rTCSAM02::getMDFR.ZScores.PrNatZ().\n");
    return(mdfr);
}
