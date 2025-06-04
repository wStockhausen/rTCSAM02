#'
#'@title Get fishery catch biomass time series from model results from TCSAM02 model runs as a dataframe
#'
#'@description Function to get fishery catch biomass time series from model results from TCSAM02 model runs as a dataframe.
#'
#'@param tcsams - single tcsam02.rep object, tcsam02.resLst object, or named list of the latter
#'@param category - 'captured','discarded','retained', 'discard mortality','total mortality', or 'index'
#'@param cast - casting formula (or NULL) for excluding x,m,s,z factor levels from an average-at-size across unspecified factors
#'@param verbose - flag (T/F) to print debug info
#'
#'@return dataframe in canonical format
#'
#'@details If 'cast' is not NULL (nor blank), the final cast'ing formula is
#'"case+process+fleet+category+type+pc+y+cast~.".
#'
#'@export
#'
getMDFR.Fisheries.CatchBiomass<-function(tcsams,
                                         category=c('captured','discarded','retained','discard mortality','total mortality','index'),
                                         cast="x",
                                         verbose=FALSE){
    if (verbose) cat("--starting rTCSAM02::getMDFR.Fisheries.CatchBiomass().\n");
    options(stringsAsFactors=FALSE);

    category<-category[1];

    if (category=='captured'){
        path<-'mr/F_list/cpB_fyxmsz'; #total captured
    } else if (category=='discarded'){
        path<-'mr/F_list/dsB_fyxmsz'; #total discarded
    } else if (category=='retained'){
        path<-'mr/F_list/rmB_fyxmsz'; #total retained
    } else if (category=='discard mortality'){
        path<-'mr/F_list/dmB_fyxmsz'; #discard MORTALITY
    } else if (category=='total mortality'){
      #retained + discard MORTALITY
      mdfr1 = getMDFR.Fisheries.CatchBiomass(tcsams,category="retained",         cast=cast,verbose=verbose);
      mdfr2 = getMDFR.Fisheries.CatchBiomass(tcsams,category="discard mortality",cast=cast,verbose=verbose);
      mdfrc = dplyr::bind_rows(mdfr1,mdfr2);
      mdfrp = mdfrc |> tidyr::pivot_wider(names_from="category",values_from="val") |>
               dplyr::rowwise() |> dplyr::mutate(val=sum(`retained`,`discard mortality`,na.rm=TRUE)) |> dplyr::ungroup() |>
               dplyr::select(!c(retained,`discard mortality`));
      mdfr<-getMDFR.CanonicalFormat(mdfrp);
    } else {
        cat("Category '",category,"' not recognized!\nReturning NULL...\n");
        return(NULL);
    }

    if (category!='total mortality'){
      mdfr<-getMDFR(path,tcsams,verbose);
      mdfr$category<-category;
      mdfr$type<-'predicted';
      mdfr<-removeImmOS(mdfr);

      castform<-"case+process+fleet+category+type+pc+y";
      if (!is.null(cast)|(cast!='')) castform<-paste0(castform,"+",cast);
      castform<-paste0(castform,"~.");
      ddfr<-reshape2::dcast(mdfr,castform,fun.aggregate=sum,na.rm=TRUE,value.var='val',drop=TRUE)
      ddfr[['.']]<-ifelse(ddfr[['.']]==0,NA,ddfr[['.']]);
      ddfr<-ddfr[!is.na(ddfr[['.']]),];#remove NA's

      mdfr<-getMDFR.CanonicalFormat(ddfr);
    }

    if (verbose) cat("--finished rTCSAM02::getMDFR.Fisheries.CatchBiomass(). \n");
    return(mdfr);
}
