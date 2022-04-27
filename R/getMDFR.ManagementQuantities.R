#'
#'@title Get management quantities from multiple model scenarios
#'
#'@description Function to get management quantities from multiple model scenarios
#'
#'@param objs - single model resLst object, or named list of them
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return dataframe
#'
#'@details Returned dataframe is in canonical format
#'
#'@importFrom dplyr inner_join mutate
#'@importFrom tibble tribble as.tibble
#'@importFrom tidyr pivot_longer
#'
#'@export
#'
getMDFR.ManagementQuantities<-function(objs,
                                       verbose=FALSE){
    options(stringsAsFactors=FALSE);

    mdfr<-NULL;
    if (class(objs)[1]=='list'){
        #objs should be a list of tcsam02 resLst objects
        for (nm in names(objs)){
            mdfrp<-getMDFR.ManagementQuantities(objs[[nm]],
                                                verbose=verbose);
            if (!is.null(mdfrp)) mdfrp$case<-nm;
            mdfr<-rbind(mdfr,mdfrp);
        }
    } else if (inherits(objs,'tcsam02.resLst')){
        if (verbose) message("--Starting rTCSAM02::getMDFR.ManagementQuantities().\n");
        #objs is a single tcsam02 resLst object
        tmp = tibble::tribble(~type,~category,
                              "OFL","catch",
                              "MSY","catch",
                              "Fofl","fishing rate",
                              "Fmsy","fishing rate",
                              "prjB","biomass",
                              "curB","biomass",
                              "B100","biomass",
                              "avgRec","recruitment");
        lstMQs = obj$rep$ptrOFLResults;
        if (!is.null(lstMQs)){
          mdfr = tibble::as.tibble(lstMQs[1:9]) %>%
                      tidyr::pivot_longer(cols=1:9,names_to="type",values_to="val") %>%
                      dplyr::inner_join(tmp,by="type") %>%
                      getMDFR.CanonicalFormat() %>%
                      dplyr::mutate(process="Management Quantities",
                                    case="tcsam02");
        }
    }

    if (verbose) message("--Finished rTCSAM02::getMDFR.ManagementQuantities().\n");
    return(mdfr);
}
