#'
#' @title Calculate selectivity function parameters for TCSAM02 based on natural mortality rates from TCSAM2013.
#'
#' @description Function to calculate selectivity function parameter values from a TCSAM2013 model for TCSAM02.
#'
#' @param obj - tcsam2013.rep object or tcsam2103.prs object
#' @param tpl - character vector with file template
#' @param verbose - flag to print debugging info
#'
#' @return file template with sel function values filled in
#'
#' @details Calculates and returns a list of selectivity function parameter values for a TCSAM02 model equivalent to those from a TCSAM2013 model run.
#'
calcTCSAM02Params.SelFcns<-function(obj,tpl,verbose=FALSE){
  if (verbose) cat("starting calcTCSAM02Params.SelFcns()\n")
  prs<-NULL;
  if (inherits(obj,"tcsam2013.prs"))    prs<-obj;
  if (inherits(obj,"tcsam2013.resLst")) prs<-obj$prs;

  #-pS1
  #--survey males
  pS1.01<-format(prs$value[prs$name==" pSrv1M_z50"],digits=10);
  pS1.02<-format(prs$value[prs$name==" pSrv2M_z50"],digits=10);
  #--survey females
  pS1.03<-format(prs$value[prs$name==" pSrv1F_z50"],digits=10);
  pS1.04<-format(prs$value[prs$name==" pSrv2F_z50"],digits=10);
  #--TCF retention
  pS1.05<-format(prs$value[prs$name==" pRetTCFM_z50A1"],digits=10);
  pS1.06<-format(prs$value[prs$name==" pRetTCFM_z50A2"],digits=10);
  #--TCF males
  mnLnZ<-prs$value[prs$name==" pSelTCFM_mnLnZ50A2"];
  idx  <-which(prs$name==" pSelTCFM_devsZ50");
  devsZ50 <-prs$value[idx];
  mnZ50A1<-mean(exp(mnLnZ+devsZ50[1:6]));
  lnMnZ50A1<-log(mnZ50A1);
  pS1.07<-format(lnMnZ50A1,digits=10);
  pS1.08<-format(prs$value[prs$name==" pSelTCFM_mnLnZ50A2"],digits=10);
  #--TCF females
  pS1.09<-format(prs$value[prs$name==" pSelTCFF_z50"],digits=10);
  #--SCF males
  pS1.10<-format(prs$value[prs$name==" pSelSCFM_z50A1"],digits=10);
  pS1.11<-format(prs$value[prs$name==" pSelSCFM_z50A2"],digits=10);
  pS1.12<-format(prs$value[prs$name==" pSelSCFM_z50A3"],digits=10);
  #--SCF females
  pS1.13<-format(prs$value[prs$name==" pSelSCFF_z50A1"],digits=10);
  pS1.14<-format(prs$value[prs$name==" pSelSCFF_z50A2"],digits=10);
  pS1.15<-format(prs$value[prs$name==" pSelSCFF_z50A3"],digits=10);
  #--GTF males
  pS1.16<-format(prs$value[prs$name==" pSelGTFM_z50A1"],digits=10);
  pS1.17<-format(prs$value[prs$name==" pSelGTFM_z50A2"],digits=10);
  pS1.18<-format(prs$value[prs$name==" pSelGTFM_z50A3"],digits=10);
  #--GTF females
  pS1.19<-format(prs$value[prs$name==" pSelGTFF_z50A1"],digits=10);
  pS1.20<-format(prs$value[prs$name==" pSelGTFF_z50A2"],digits=10);
  pS1.21<-format(prs$value[prs$name==" pSelGTFF_z50A3"],digits=10);
  #--RKF males
  pS1.22<-format(prs$value[prs$name==" pSelRKFM_z50A1"],digits=10);
  pS1.23<-format(prs$value[prs$name==" pSelRKFM_z50A2"],digits=10);
  pS1.24<-format(prs$value[prs$name==" pSelRKFM_z50A3"],digits=10);
  #--RKF females
  pS1.25<-format(prs$value[prs$name==" pSelRKFF_z50A1"],digits=10);
  pS1.26<-format(prs$value[prs$name==" pSelRKFF_z50A2"],digits=10);
  pS1.27<-format(prs$value[prs$name==" pSelRKFF_z50A3"],digits=10);
  tpl<-gsub("&&pS1.01",pS1.01,tpl,fixed=TRUE);
  tpl<-gsub("&&pS1.02",pS1.02,tpl,fixed=TRUE);
  tpl<-gsub("&&pS1.03",pS1.03,tpl,fixed=TRUE);
  tpl<-gsub("&&pS1.04",pS1.04,tpl,fixed=TRUE);
  tpl<-gsub("&&pS1.05",pS1.05,tpl,fixed=TRUE);
  tpl<-gsub("&&pS1.06",pS1.06,tpl,fixed=TRUE);
  tpl<-gsub("&&pS1.07",pS1.07,tpl,fixed=TRUE);
  tpl<-gsub("&&pS1.08",pS1.08,tpl,fixed=TRUE);
  tpl<-gsub("&&pS1.09",pS1.09,tpl,fixed=TRUE);
  tpl<-gsub("&&pS1.10",pS1.10,tpl,fixed=TRUE);
  tpl<-gsub("&&pS1.11",pS1.11,tpl,fixed=TRUE);
  tpl<-gsub("&&pS1.12",pS1.12,tpl,fixed=TRUE);
  tpl<-gsub("&&pS1.13",pS1.13,tpl,fixed=TRUE);
  tpl<-gsub("&&pS1.14",pS1.14,tpl,fixed=TRUE);
  tpl<-gsub("&&pS1.15",pS1.15,tpl,fixed=TRUE);
  tpl<-gsub("&&pS1.16",pS1.16,tpl,fixed=TRUE);
  tpl<-gsub("&&pS1.17",pS1.17,tpl,fixed=TRUE);
  tpl<-gsub("&&pS1.18",pS1.18,tpl,fixed=TRUE);
  tpl<-gsub("&&pS1.19",pS1.19,tpl,fixed=TRUE);
  tpl<-gsub("&&pS1.20",pS1.20,tpl,fixed=TRUE);
  tpl<-gsub("&&pS1.21",pS1.21,tpl,fixed=TRUE);
  tpl<-gsub("&&pS1.22",pS1.22,tpl,fixed=TRUE);
  tpl<-gsub("&&pS1.23",pS1.23,tpl,fixed=TRUE);
  tpl<-gsub("&&pS1.24",pS1.24,tpl,fixed=TRUE);
  tpl<-gsub("&&pS1.25",pS1.25,tpl,fixed=TRUE);
  tpl<-gsub("&&pS1.26",pS1.26,tpl,fixed=TRUE);
  tpl<-gsub("&&pS1.27",pS1.27,tpl,fixed=TRUE);

  #--pS2:
  #--survey males
  pS2.01<-format(prs$value[prs$name==" pSrv1M_dz5095"],digits=10);
  pS2.02<-format(prs$value[prs$name==" pSrv2M_dz5095"],digits=10);
  #--survey females
  pS2.03<-format(prs$value[prs$name==" pSrv1F_dz5095"],digits=10);
  pS2.04<-format(prs$value[prs$name==" pSrv2F_dz5095"],digits=10);
  #--TCF retention
  pS2.05<-format(prs$value[prs$name==" pRetTCFM_slpA1"],digits=10);
  pS2.06<-format(prs$value[prs$name==" pRetTCFM_slpA2"],digits=10);
  #--TCF males
  pS2.07<-format(prs$value[prs$name==" pSelTCFM_slpA1"],digits=10);
  pS2.08<-format(prs$value[prs$name==" pSelTCFM_slpA2"],digits=10);
  #--TCF females
  pS2.09<-format(prs$value[prs$name==" pSelTCFF_slp"],digits=10);
  #--SCF males
  pS2.10<-format(prs$value[prs$name==" pSelSCFM_slpA1"],digits=10);
  pS2.11<-format(prs$value[prs$name==" pSelSCFM_slpA2"],digits=10);
  pS2.12<-format(prs$value[prs$name==" pSelSCFM_slpA3"],digits=10);
  #--SCF females
  pS2.13<-format(prs$value[prs$name==" pSelSCFF_slpA1"],digits=10);
  pS2.14<-format(prs$value[prs$name==" pSelSCFF_slpA2"],digits=10);
  pS2.15<-format(prs$value[prs$name==" pSelSCFF_slpA3"],digits=10);
  #--GTF males
  pS2.16<-format(prs$value[prs$name==" pSelGTFM_slpA1"],digits=10);
  pS2.17<-format(prs$value[prs$name==" pSelGTFM_slpA2"],digits=10);
  pS2.18<-format(prs$value[prs$name==" pSelGTFM_slpA3"],digits=10);
  #--GTF females
  pS2.19<-format(prs$value[prs$name==" pSelGTFF_slpA1"],digits=10);
  pS2.20<-format(prs$value[prs$name==" pSelGTFF_slpA2"],digits=10);
  pS2.21<-format(prs$value[prs$name==" pSelGTFF_slpA3"],digits=10);
  #--RKF males
  pS2.22<-format(prs$value[prs$name==" pSelRKFM_slpA1"],digits=10);
  pS2.23<-format(prs$value[prs$name==" pSelRKFM_slpA2"],digits=10);
  pS2.24<-format(prs$value[prs$name==" pSelRKFM_slpA3"],digits=10);
  #--RKF females
  pS2.25<-format(prs$value[prs$name==" pSelRKFF_slpA1"],digits=10);
  pS2.26<-format(prs$value[prs$name==" pSelRKFF_slpA2"],digits=10);
  pS2.27<-format(prs$value[prs$name==" pSelRKFF_slpA3"],digits=10);
  tpl<-gsub("&&pS2.01",pS2.01,tpl,fixed=TRUE);
  tpl<-gsub("&&pS2.02",pS2.02,tpl,fixed=TRUE);
  tpl<-gsub("&&pS2.03",pS2.03,tpl,fixed=TRUE);
  tpl<-gsub("&&pS2.04",pS2.04,tpl,fixed=TRUE);
  tpl<-gsub("&&pS2.05",pS2.05,tpl,fixed=TRUE);
  tpl<-gsub("&&pS2.06",pS2.06,tpl,fixed=TRUE);
  tpl<-gsub("&&pS2.07",pS2.07,tpl,fixed=TRUE);
  tpl<-gsub("&&pS2.08",pS2.08,tpl,fixed=TRUE);
  tpl<-gsub("&&pS2.09",pS2.09,tpl,fixed=TRUE);
  tpl<-gsub("&&pS2.10",pS2.10,tpl,fixed=TRUE);
  tpl<-gsub("&&pS2.11",pS2.11,tpl,fixed=TRUE);
  tpl<-gsub("&&pS2.12",pS2.12,tpl,fixed=TRUE);
  tpl<-gsub("&&pS2.13",pS2.13,tpl,fixed=TRUE);
  tpl<-gsub("&&pS2.14",pS2.14,tpl,fixed=TRUE);
  tpl<-gsub("&&pS2.15",pS2.15,tpl,fixed=TRUE);
  tpl<-gsub("&&pS2.16",pS2.16,tpl,fixed=TRUE);
  tpl<-gsub("&&pS2.17",pS2.17,tpl,fixed=TRUE);
  tpl<-gsub("&&pS2.18",pS2.18,tpl,fixed=TRUE);
  tpl<-gsub("&&pS2.19",pS2.19,tpl,fixed=TRUE);
  tpl<-gsub("&&pS2.20",pS2.20,tpl,fixed=TRUE);
  tpl<-gsub("&&pS2.21",pS2.21,tpl,fixed=TRUE);
  tpl<-gsub("&&pS2.22",pS2.22,tpl,fixed=TRUE);
  tpl<-gsub("&&pS2.23",pS2.23,tpl,fixed=TRUE);
  tpl<-gsub("&&pS2.24",pS2.24,tpl,fixed=TRUE);
  tpl<-gsub("&&pS2.25",pS2.25,tpl,fixed=TRUE);
  tpl<-gsub("&&pS2.26",pS2.26,tpl,fixed=TRUE);
  tpl<-gsub("&&pS2.27",pS2.27,tpl,fixed=TRUE);

  #-pS3:
  #--SCF males
  pS3.1<-format(prs$value[prs$name==" pSelSCFM_lnZ50D1"],digits=10);
  pS3.2<-format(prs$value[prs$name==" pSelSCFM_lnZ50D2"],digits=10);
  pS3.3<-format(prs$value[prs$name==" pSelSCFM_lnZ50D3"],digits=10);
  tpl<-gsub("&&pS3.1",pS3.1,tpl,fixed=TRUE);
  tpl<-gsub("&&pS3.2",pS3.2,tpl,fixed=TRUE);
  tpl<-gsub("&&pS3.3",pS3.3,tpl,fixed=TRUE);

  #-pS4:
  #--SCF males
  pS4.1<-format(prs$value[prs$name==" pSelSCFM_slpD1"],digits=10);
  pS4.2<-format(prs$value[prs$name==" pSelSCFM_slpD2"],digits=10);
  pS4.3<-format(prs$value[prs$name==" pSelSCFM_slpD3"],digits=10);
  tpl<-gsub("&&pS4.1",pS4.1,tpl,fixed=TRUE);
  tpl<-gsub("&&pS4.2",pS4.2,tpl,fixed=TRUE);
  tpl<-gsub("&&pS4.3",pS4.3,tpl,fixed=TRUE);

  #--pDevsS1
  pDevsS1<-format(prs$value[prs$name==" pSelTCFM_devsZ50"],digits=10);
  tpl<-gsub("&&pDevsS1",paste(pDevsS1,collapse=" "),tpl,fixed=TRUE);

  if (verbose) cat("finished calcTCSAM02Params.SelFcns()\n")
  return(tpl);
}
