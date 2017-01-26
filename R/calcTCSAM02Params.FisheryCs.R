#'
#' @title Calculate fishery catchability parameters for TCSAM02 based on values from a TCSAM2013 model.
#'
#' @description Function to calculate fishery catchability parameter values for TCSAM02 based on values from a TCSAM2013 model.
#'
#' @param obj - tcsam2013.resLst object or tcsam2103.prs object
#' @param tpl - character vector with file template
#' @param verbose - flag to print debugging info
#'
#' @return file template with fishery catchability parameter values filled in
#'
#' @details Calculates and returns a list of survey Q parameter values for a TCSAM02 model equivalent to those from a TCSAM2013 model run.
#'
calcTCSAM02Params.FisheryCs<-function(obj,tpl,verbose=FALSE){
  if (verbose) cat("starting calcTCSAM02Params.FisheryCs()\n")
  prs<-NULL;
  if (inherits(obj,"tcsam2013.prs"))    prs<-obj;
  if (inherits(obj,"tcsam2013.resLst")) prs<-obj$prs;

  #TCSAM2013 parameter values
  pAvgLnF_TCF<-prs$value[prs$name==" pAvgLnF_TCF"];
  pAvgLnF_SCF<-prs$value[prs$name==" pAvgLnF_SCF"];
  pAvgLnF_RKF<-prs$value[prs$name==" pAvgLnF_RKF"];
  pAvgLnF_GTF<-prs$value[prs$name==" pAvgLnF_GTF"];
  pAvgLnF_TCFF<-prs$value[prs$name==" pAvgLnF_TCFF"];
  pAvgLnF_SCFF<-prs$value[prs$name==" pAvgLnF_SCFF"];
  pAvgLnF_RKFF<-prs$value[prs$name==" pAvgLnF_RKFF"];
  pAvgLnF_GTFF<-prs$value[prs$name==" pAvgLnF_GTFF"];
  pF_DevsTCF<-prs$value[prs$name==" pF_DevsTCF"];
  pF_DevsSCF<-prs$value[prs$name==" pF_DevsSCF"];
  pF_DevsGTF<-prs$value[prs$name==" pF_DevsGTF"];
  pF_DevsRKF<-prs$value[prs$name==" pF_DevsRKF"];

  mnF_GTF<-mean(exp(pAvgLnF_GTF+pF_DevsGTF)); #mean F 1973+

  #TCSAM02 values
  pLnC.1 <- format(log(0.05),digits=10);    #TCF pre-1965
  pLnC.2 <- format(pAvgLnF_TCF,digits=10);
  pLnC.3 <- format(log(0.01),digits=10);    #SCF pre-1978
  pLnC.4 <- format(pAvgLnF_SCF,digits=10);
  pLnC.5 <- format(log(mnF_GTF),digits=10); #GTF pre-1973
  pLnC.6 <- format(pAvgLnF_GTF,digits=10);
  pLnC.7 <- format(log(0.02),digits=10);    #RKF pre-1953
  pLnC.8 <- format(pAvgLnF_RKF,digits=10);

  pLnDCX.1 <- format(pAvgLnF_TCFF,digits=10);
  pLnDCX.2 <- format(pAvgLnF_SCFF,digits=10);
  pLnDCX.3 <- format(pAvgLnF_GTFF,digits=10);
  pLnDCX.4 <- format(pAvgLnF_RKFF,digits=10);

  pDevsLnC.1 <- format(paste(pF_DevsTCF,collapse=" "),digits=10);
  pDevsLnC.2 <- format(paste(pF_DevsSCF,collapse=" "),digits=10);
  pDevsLnC.3 <- format(paste(pF_DevsGTF,collapse=" "),digits=10);
  pDevsLnC.4 <- format(paste(pF_DevsRKF,collapse=" "),digits=10);

  if (verbose){
    cat("pLnC.1 = ",pLnC.1,"\n")
    cat("pLnC.2 = ",pLnC.2,"\n")
    cat("pLnC.3 = ",pLnC.3,"\n")
    cat("pLnC.4 = ",pLnC.4,"\n")
    cat("pLnC.5 = ",pLnC.5,"\n")
    cat("pLnC.6 = ",pLnC.6,"\n")
    cat("pLnC.7 = ",pLnC.7,"\n")
    cat("pLnC.8 = ",pLnC.8,"\n\n")
    cat("pLnDCX.1 = ",pLnDCX.1,"\n")
    cat("pLnDCX.2 = ",pLnDCX.2,"\n")
    cat("pLnDCX.3 = ",pLnDCX.3,"\n")
    cat("pLnDCX.4 = ",pLnDCX.4,"\n\n")
    cat("pDevsLnC.1 = ",pDevsLnC.1,"\n")
    cat("pDevsLnC.2 = ",pDevsLnC.2,"\n")
    cat("pDevsLnC.3 = ",pDevsLnC.3,"\n")
    cat("pDevsLnC.4 = ",pDevsLnC.4,"\n")
  }

  tpl<-gsub("&&pLnC.1",pLnC.1,tpl,fixed=TRUE);
  tpl<-gsub("&&pLnC.2",pLnC.2,tpl,fixed=TRUE);
  tpl<-gsub("&&pLnC.3",pLnC.3,tpl,fixed=TRUE);
  tpl<-gsub("&&pLnC.4",pLnC.4,tpl,fixed=TRUE);
  tpl<-gsub("&&pLnC.5",pLnC.5,tpl,fixed=TRUE);
  tpl<-gsub("&&pLnC.6",pLnC.6,tpl,fixed=TRUE);
  tpl<-gsub("&&pLnC.7",pLnC.7,tpl,fixed=TRUE);
  tpl<-gsub("&&pLnC.8",pLnC.8,tpl,fixed=TRUE);

  tpl<-gsub("&&pLnDCX.1",pLnDCX.1,tpl,fixed=TRUE);
  tpl<-gsub("&&pLnDCX.2",pLnDCX.2,tpl,fixed=TRUE);
  tpl<-gsub("&&pLnDCX.3",pLnDCX.3,tpl,fixed=TRUE);
  tpl<-gsub("&&pLnDCX.4",pLnDCX.4,tpl,fixed=TRUE);

  tpl<-gsub("&&pDevsLnC.1",pDevsLnC.1,tpl,fixed=TRUE);
  tpl<-gsub("&&pDevsLnC.2",pDevsLnC.2,tpl,fixed=TRUE);
  tpl<-gsub("&&pDevsLnC.3",pDevsLnC.3,tpl,fixed=TRUE);
  tpl<-gsub("&&pDevsLnC.4",pDevsLnC.4,tpl,fixed=TRUE);

  if (verbose) cat("finished calcTCSAM02Params.FisheryCs()\n")
  return(tpl)
}
