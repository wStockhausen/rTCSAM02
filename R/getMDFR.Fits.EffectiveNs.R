#'
#'@title Get effective N's for fleet size compositions
#'
#'@description Function to get effective N's for fleet size compositions.
#'
#'@param objs - single model report list object, or named list of them
#'@param fleet.type - fleet type ('fishery' or 'survey')
#'@param catch.type - cath type ('index','total','discard', or 'retained')
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return dataframe
#'
#'@details Returned dataframe is in canonical format
#'
#'@export
#'
getMDFR.Fits.EffectiveNs<-function(objs,
                                   fleet.type=c('survey','fishery'),
                                   catch.type=c('index','total','discard','retained'),
                                   verbose=FALSE){
    if (verbose) {
        cat("--Starting rTCSAM02::getMDFR.Fits.EffectiveN().\n");
        cat("Extracting fleet.type = ",fleet.type,", catch.type = ",catch.type,"\n");
    }
    options(stringsAsFactors=FALSE);

    fleet.type<-fleet.type[1];
    catch.type<-catch.type[1];

    mdfr<-NULL;
    if (class(objs)[1]=='list'){
        #repObjs should be a list of tcsam02 resLst objects
        for (nm in names(objs)){
            if (verbose) cat("Processing ",nm,"\n");
            mdfrp<-getMDFR.Fits.EffectiveNs(objs[[nm]],
                                            fleet.type=fleet.type,
                                            catch.type=catch.type,
                                            verbose=verbose);
            if (!is.null(mdfrp)) mdfrp$case<-nm;
            mdfr<-rbind(mdfr,mdfrp);
            rm(mdfrp);
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
            cat("RETURNING NULL from rTCSAM02::getMDFR.Fits.EffectiveNs()!!")
            return(NULL);
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
                        if (!is.null(ct$n.at.z)){
                            if (verbose) cat("---Getting effective n's.\n")
                            mdfrp<-getMDFR.EffectiveNsForSizeComps(ct$n.at.z,objs$rep$mc,verbose=verbose);
                            if (!is.null(mdfrp)) mdfrp$category<-"effective Ns";
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

    if (!is.null(mdfr)) mdfr<-getMDFR.CanonicalFormat(mdfr);
    if (verbose) cat("--Finished getMDFR.Fits.EffectiveNs().\n");
    return(mdfr);
}
