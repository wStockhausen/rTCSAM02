#'
#'@title Get a TCSAM02 projections list by reading a TCSAM02 calcProjections.R file
#'
#'@description Function to get a TCSAM02 projections list by reading a TCSAM02 calcProjections.R file.
#'
#'@param repFile - projections file from a TCSAM02 model run to source (default="calcProjections.final.R")
#'@param verbose - flag (T/F) to prnt diagnostic info
#'
#'@return a TCSAM02 projections list, or NULL. The returned object will be a list of class 'tcsam02.projList'.
#'
#'@details The returned object will be a list of class 'tcsam02.projList'.
#'
#'@export
#'
getProjectionsList<-function(fn){
  source(fn,local=TRUE);
  class(projFMsList)<-c('tcsam02.projList',class(projFMsList));
  return(projFMsList);
}

#'
#'@title Get a TCSAM02 projections list (a tcsam02.projList object) as a [tibble::tibble]
#'
#'@description Function to get a TCSAM02 projections list (a tcsam02.projList object) as a [tibble::tibble].
#'
#'@param projList - projections list parsed using [getProjectionsList(projFile)]
#'
#'@return a [tibble::tibble] representing a TCSAM02 projections list, or NULL. The returned object will be of class 'tcsam02.projDFR'.
#'
#'@details The returned object will be a [tibble::tibble] of class 'tcsam02.projDFR'.
#'
#'@export
#'
getMDFR.Projections<-function(projList){
  yr    = projList$yr;
  nYrs  = projList$nYrs;
  nReps = projList$nReps;
  Fmsy  = projList$Fmsy;
  Bmsy  = projList$Bmsy;
  Fofl  = projList$Fofl;
  lstFs = projList$listFs;
  tblFs=list(); ctrFs=0;
  for (lstF in lstFs){
    prjF = lstF$prjF;
    relF = lstF$mult;
    lstReps = lstF$reps;
    tblReps=list(); ctrReps=0;
    for (lstRep in lstReps){
      ctrReps = ctrReps+1;
      tblReps[[ctrReps]] = tibble::tibble(rep=lstRep$rep,
                                          y=yr+0:(nYrs-1),
                                          rec=lstRep$recs,
                                          MMB=lstRep$MMB,
                                          MFB=lstRep$MFB);
    }#--lstReps
    ctrFs = ctrFs+1;
    tblFs[[ctrFs]] = dplyr::bind_cols(prjF=prjF,
                                      relF=relF,
                                      dplyr::bind_rows(tblReps));
  }#--lstFs
  mdfr = dplyr::bind_cols(Fmsy=Fmsy,Bmsy=Bmsy,Fofl=Fofl,
                          dplyr::bind_rows(tblFs));
  class(mdfr)<-c('tcsam02.projDFR',class(mdfr));
  return(mdfr);
}
