writeOldProjectionModelFile<-function(rep){
    oflRes<-rep$oflResults;

    mc<-rep$mc;
    mnYr<-as.character(mc$dims$y$mny+1);
    mxYr<-as.character(mc$dims$y$mxy);
    asYr<-as.character(mc$dims$y$asy);
    vMnYr<-mc$dims$y$mny+1;
    vMxYr<-mc$dims$y$mxy;
    vAsYr<-mc$dims$y$asy;
      MALE <- 'male'
    FEMALE <- 'female';
    IMMATURE <- 'immature';
      MATURE <-   'mature';
    NEW_SHELL <- "new shell";
    OLD_SHELL <- "old shell";
  # stuff for input to projection model
      cat("#--------------------------------------------------------------------","\n");
      cat("#FOR WTS PROJECTION MODEL--------------------------------------------","\n");
      cat("#--------------------------------------------------------------------","\n");
      cat(mnYr,"\t\t#start year for assessment model","\n");
      cat(asYr,"\t\t#end year for assessment model/start year for projections","\n");
      cat(10   ,"\t\t#number of years for projections","\n");
      cat(1000 ,"\t\t#number of projections to make","\n");
      cat(mc$dims$z$n,"\t\t#number of size bins in model","\n");
      cat("#---------------------------","\n");
      cat("#reference point calculations","\n");
      cat("#---------------------------","\n");
      cat(0.35,"\t\t#target SBPR reduction ratio (e.g. 0.35)","\n");
      cat(mean(rep$mp$R_list$R_y[as.character(1981:vMxYr)])*1000,"\t\t#total average recruitment for BXX/Bmsy calculation (1000's of recruits)","\n");
      cat(rep$mr$P_list$MB_yx[mxYr,"male"],"\t\t#'current' spawning biomass (MMB, 1000's t) ","\n");
      cat("???","\t\t#cv of 'current' spawning biomass","\n");
      cat(1    ,"\t\t#harvest strategy","\n");
      cat(0.1  ,"\t\t#alpha for control rule","\n");
      cat(0.25 ,"\t\t#beta  for control rule","\n");
      cat("#---------------------------","\n");
      cat("#SRR info","\n");
      cat("#---------------------------","\n");
      cat("1"   ,"\t\t#srType  : flag indicating type of SRR","\n");
      cat("0"   ,"\t\t#recGamma: recruitment autocorrelation parameter","\n");
      cat(-1    ,"\t\t#recDep  : recruitment depensation flag","\n");
      cat(-1    ,"\t\t#inpRecH : input value for steepness","\n");
      cat(-1    ,"\t\t#inpRecR0: input value for virgin recruitment","\n");
      cat("#---------------------------","\n");
      cat("#Recruitment and spawning biomass time series","\n");
      cat("#---------------------------","\n");
      cat(1982  ,"\t\t#mMnYrForRecAvg: min assessment model year for averaging recruitment to estimate BXX, Bmsy","\n");
      cat(asYr ,"\t\t#mMxYrForRecAvg: max assessment model year for averaging recruitment to estimate BXX, Bmsy","\n");
      cat("#asmtModRec(nSXs,mMnYr,mMxYr): unlagged recruitments female, male start year to asYr from model (1000's)","\n");
      cat( 0.5*rep$mp$R_list$R_y*1000 ,"\n");#females;
      cat( 0.5*rep$mp$R_list$R_y*1000 ,"\n");#  males;
      cat("#asmtModSpB(mMnYr,mMxYr-1): male spawning biomass at matetime (1000's t) for str year to mxYr for spawner recruit curve to estimate recruitments","\n");
      cat(rep$mr$P_list$MB_yx[(vMnYr+1):vMxYr,'male'],"\n");
      cat("#---------------------------","\n");
      cat("#Pop info in last year of assessment model","\n");
      cat("#---------------------------","\n");
      cat("#numbers at size immature new shell female, male in final year (1000's)","\n");
      cat(rep$mr$P_list$N_yxmsz[asYr,FEMALE,IMMATURE,NEW_SHELL,]*1000,"\n");
      cat(rep$mr$P_list$N_yxmsz[asYr,  MALE,IMMATURE,NEW_SHELL,]*1000,"\n");
      cat("#numbers at length immature old shell female male last year (1000's)","\n");
      cat(rep$mr$P_list$N_yxmsz[asYr,FEMALE,IMMATURE,OLD_SHELL,]*1000,"\n");
      cat(rep$mr$P_list$N_yxmsz[asYr,  MALE,IMMATURE,OLD_SHELL,]*1000,"\n");
      cat("#numbers at length mature new shell female male last year (1000's)","\n");
      cat(rep$mr$P_list$N_yxmsz[asYr,FEMALE,  MATURE,NEW_SHELL,]*1000,"\n");
      cat(rep$mr$P_list$N_yxmsz[asYr,  MALE,  MATURE,NEW_SHELL,]*1000,"\n");
      cat("#numbers at length mature old shell female male last year (1000's) ","\n");
      cat(rep$mr$P_list$N_yxmsz[asYr,FEMALE,  MATURE,OLD_SHELL,]*1000,"\n");
      cat(rep$mr$P_list$N_yxmsz[asYr,  MALE,  MATURE,OLD_SHELL,]*1000,"\n");
      cat("#---------------------------","\n");
      cat("#Fisheries info","\n");
      cat("#---------------------------","\n");
      cat("#time of catch in fraction of year from survey - 7 months","\n");
      cat(rep$data$bio$timing[mxYr,'midptFisheries'],"\n");#IMPORTANT CHANGE; was asYr

      cat(0,"\t\t#inpFmTCF: input F for directed Tanner crab fishing mortality","\n");
      cat(oflRes$catchInfoM$capF_fms[2,"mature","new shell"],"\t\t#inpFmSCF: input male F for snow crab fishing mortality","\n");
      cat(oflRes$catchInfoM$capF_fms[4,"mature","new shell"],"\t\t#inpFmRKF: input male F for BBRKC  fishing mortality","\n");
      cat(oflRes$catchInfoM$capF_fms[3,"mature","new shell"],"\t\t#inpFmGTF: input male F for groundfish fishery fishing mortality","\n");

      cat(log(oflRes$catchInfoF$capF_fms[1,"mature","new shell"]/oflRes$catchInfoM$capF_fms[1,"mature","new shell"]),"\t\t#pAvgLnF_TCFF: ln-scale offset to F for female bycatch in the directed Tanner crab fishery","\n");
      cat(log(oflRes$catchInfoF$capF_fms[2,"mature","new shell"]/oflRes$catchInfoM$capF_fms[2,"mature","new shell"]),"\t\t#pAvgLnF_SCFF: ln-scale offset to F for female bycatch in the snow crab fishery","\n");
      cat(log(oflRes$catchInfoF$capF_fms[4,"mature","new shell"]/oflRes$catchInfoM$capF_fms[4,"mature","new shell"]),"\t\t#pAvgLnF_RKFF: ln-scale offset to F for female bycatch in the BBRKC fishery","\n");
      cat(log(oflRes$catchInfoF$capF_fms[3,"mature","new shell"]/oflRes$catchInfoM$capF_fms[3,"mature","new shell"]),"\t\t#pAvgLnF_GTFF: ln-scale offset to F for female bycatch in the groundfish fishery","\n");

      cat("#selTCF_TotMale(nSCs,nSXs): average of last 4 years selTCFM_syz total male new old shell","\n");
      cat((selTCFM_syz(NEW_SHELL,asYr-4)+selTCFM_syz(NEW_SHELL,asYr-3)+selTCFM_syz(NEW_SHELL,asYr-2)+selTCFM_syz(NEW_SHELL,mxYr))/4.0,"\n");
      cat((selTCFM_syz(OLD_SHELL,asYr-4)+selTCFM_syz(OLD_SHELL,asYr-3)+selTCFM_syz(OLD_SHELL,asYr-2)+selTCFM_syz(OLD_SHELL,mxYr))/4.0,"\n");
      if (optFM==0){
            cat("#selTCF_RetMale(nSCs): average of last 4 years selTCFM_syz retained curve male new old shell","\n");
            cat((selTCFR_syz(NEW_SHELL,asYr-4)+selTCFR_syz(NEW_SHELL,asYr-3)+selTCFR_syz(NEW_SHELL,asYr-2)+selTCFR_syz(NEW_SHELL,mxYr))/4.0,"\n");#IMPORTANT CHANGE: was only over last 3 years (but said 4)
            cat((selTCFR_syz(OLD_SHELL,asYr-4)+selTCFR_syz(OLD_SHELL,asYr-3)+selTCFR_syz(OLD_SHELL,asYr-2)+selTCFR_syz(OLD_SHELL,mxYr))/4.0,"\n");
      } else {
            cat("#retFcn_syz(nSCs): average of last four years","\n");
            cat((retFcn_syz(NEW_SHELL,asYr-4)+retFcn_syz(NEW_SHELL,asYr-3)+retFcn_syz(NEW_SHELL,asYr-2)+retFcn_syz(NEW_SHELL,mxYr))/4.0,"\n");#IMPORTANT CHANGE: was only over last 3 years (but said 4)
            cat((retFcn_syz(OLD_SHELL,asYr-4)+retFcn_syz(OLD_SHELL,asYr-3)+retFcn_syz(OLD_SHELL,asYr-2)+retFcn_syz(OLD_SHELL,mxYr))/4.0,"\n");
      }
      cat("#selTCF_TotMaleEast(nSCs,nSXs): set same as average total","\n");
      cat((selTCFM_syz(NEW_SHELL,asYr-4)+selTCFM_syz(NEW_SHELL,asYr-3)+selTCFM_syz(NEW_SHELL,asYr-2)+selTCFM_syz(NEW_SHELL,mxYr))/4.0,"\n");
      cat((selTCFM_syz(OLD_SHELL,asYr-4)+selTCFM_syz(OLD_SHELL,asYr-3)+selTCFM_syz(OLD_SHELL,asYr-2)+selTCFM_syz(OLD_SHELL,mxYr))/4.0,"\n");
#      cat("#selTCF_RetMaleEast(nSCs,nSXs): set same as avg retained","\n");
#      cat((selTCFR_syz(NEW_SHELL,asYr-4)+selTCFR_syz(NEW_SHELL,asYr-3)+selTCFR_syz(NEW_SHELL,asYr-2)+selTCFR_syz(NEW_SHELL,mxYr))/4.0,"\n");#IMPORTANT CHANGE: was only over last 3 years (but said 4)
#      cat((selTCFR_syz(OLD_SHELL,asYr-4)+selTCFR_syz(OLD_SHELL,asYr-3)+selTCFR_syz(OLD_SHELL,asYr-2)+selTCFR_syz(OLD_SHELL,mxYr))/4.0,"\n");
      if (optFM==0){
            cat("#selTCF_RetMaleEast(nSCs,nSXs): set same as avg retained","\n");
            cat((selTCFR_syz(NEW_SHELL,asYr-4)+selTCFR_syz(NEW_SHELL,asYr-3)+selTCFR_syz(NEW_SHELL,asYr-2)+selTCFR_syz(NEW_SHELL,mxYr))/4.0,"\n");#IMPORTANT CHANGE: was only over last 3 years (but said 4)
            cat((selTCFR_syz(OLD_SHELL,asYr-4)+selTCFR_syz(OLD_SHELL,asYr-3)+selTCFR_syz(OLD_SHELL,asYr-2)+selTCFR_syz(OLD_SHELL,mxYr))/4.0,"\n");
      } else {
            cat("#retFcn_syz(nSCs) for East: same as average retained","\n");
            cat((retFcn_syz(NEW_SHELL,asYr-4)+retFcn_syz(NEW_SHELL,asYr-3)+retFcn_syz(NEW_SHELL,asYr-2)+retFcn_syz(NEW_SHELL,mxYr))/4.0,"\n");#IMPORTANT CHANGE: was only over last 3 years (but said 4)
            cat((retFcn_syz(OLD_SHELL,asYr-4)+retFcn_syz(OLD_SHELL,asYr-3)+retFcn_syz(OLD_SHELL,asYr-2)+retFcn_syz(OLD_SHELL,mxYr))/4.0,"\n");
      }
      cat("#selTCF_TotMaleWest(nSCs,nSXs): set same as average total","\n");
      cat((selTCFM_syz(NEW_SHELL,asYr-4)+selTCFM_syz(NEW_SHELL,asYr-3)+selTCFM_syz(NEW_SHELL,asYr-2)+selTCFM_syz(NEW_SHELL,mxYr))/4.0,"\n");
      cat((selTCFM_syz(OLD_SHELL,asYr-4)+selTCFM_syz(OLD_SHELL,asYr-3)+selTCFM_syz(OLD_SHELL,asYr-2)+selTCFM_syz(OLD_SHELL,mxYr))/4.0,"\n");
#      cat("#selTCF_RetMaleWest(nSCs,nSXs): SET SAME AS AVG RETAINED, BUT SHIFTED TO LOWER END BY 10 mm","\n");
#      cat((selTCFR_syz(NEW_SHELL,asYr-4)+selTCFR_syz(NEW_SHELL,asYr-3)+selTCFR_syz(NEW_SHELL,asYr-2)+selTCFR_syz(NEW_SHELL,mxYr))/4.0,"\n");#IMPORTANT CHANGE: was only over last 3 years (but said 4)
#      cat((selTCFR_syz(OLD_SHELL,asYr-4)+selTCFR_syz(OLD_SHELL,asYr-3)+selTCFR_syz(OLD_SHELL,asYr-2)+selTCFR_syz(OLD_SHELL,mxYr))/4.0,"\n");
      if (optFM==0){
            cat("#selTCF_RetMaleWest(nSCs,nSXs): SET SAME AS AVG RETAINED, BUT SHIFTED TO LOWER END BY 10 mm","\n");
            cat((selTCFR_syz(NEW_SHELL,asYr-4)+selTCFR_syz(NEW_SHELL,asYr-3)+selTCFR_syz(NEW_SHELL,asYr-2)+selTCFR_syz(NEW_SHELL,mxYr))/4.0,"\n");#IMPORTANT CHANGE: was only over last 3 years (but said 4)
            cat((selTCFR_syz(OLD_SHELL,asYr-4)+selTCFR_syz(OLD_SHELL,asYr-3)+selTCFR_syz(OLD_SHELL,asYr-2)+selTCFR_syz(OLD_SHELL,mxYr))/4.0,"\n");
      } else {
            cat("#retFcn_syz(nSCs) for West: same as average retained","\n");
            cat((retFcn_syz(NEW_SHELL,asYr-4)+retFcn_syz(NEW_SHELL,asYr-3)+retFcn_syz(NEW_SHELL,asYr-2)+retFcn_syz(NEW_SHELL,mxYr))/4.0,"\n");#IMPORTANT CHANGE: was only over last 3 years (but said 4)
            cat((retFcn_syz(OLD_SHELL,asYr-4)+retFcn_syz(OLD_SHELL,asYr-3)+retFcn_syz(OLD_SHELL,asYr-2)+retFcn_syz(OLD_SHELL,mxYr))/4.0,"\n");
      }
      cat("#selTCF_Female(nZs): selectivity for females in directed fishery","\n");
      cat(selTCFF_z,"\n");
      cat("#selSCF_cxz(nSXs,nZs): selectivity in snow crab fishery","\n");
      cat(selSCF_cxz(3,FEMALE),"\n");
      cat(selSCF_cxz(3,  MALE),"\n");
      cat("#selRKF_cxz(nSXs,nZs): selectivity in BBRKC fishery","\n");
      cat(selRKF_cxz(3,FEMALE),"\n");
      cat(selRKF_cxz(3,  MALE),"\n");
      cat("#selGTF_cxz(nSXs,nZs): selectivity in groundfish fishery","\n");
      cat(selGTF_cxz(3),"\n");

      cat("#---------------------------","\n");
      cat("#Biological info","\n");
      cat("#---------------------------","\n");
      cat("#M_f(nSCs,nMSs): natural mortality for females","\n");
      cat(M_msx(IMMATURE,NEW_SHELL,FEMALE),",\n",M_msx(MATURE,NEW_SHELL,FEMALE),"\n");
      cat(M_msx(IMMATURE,OLD_SHELL,FEMALE),",\n",M_msx(MATURE,OLD_SHELL,FEMALE),"\n");
      cat("#M_m(nSCs,nMSs): natural mortality for males","\n");
      cat(M_msx(IMMATURE,NEW_SHELL,  MALE),",\n",M_msx(MATURE,NEW_SHELL,  MALE),"\n");
      cat(M_msx(IMMATURE,OLD_SHELL,  MALE),",\n",M_msx(MATURE,OLD_SHELL,  MALE),"\n");
      cat("#weight at length female juvenile (t)","\n");
      cat(wt_xmz(FEMALE)(IMMATURE)*0.001,"\n");
      cat("#weight at length female mature (t)","\n");
      cat(wt_xmz(FEMALE)(MATURE)*0.001,"\n");
      cat("#weight at length male (t)","\n");
      cat(wt_xmz(MALE,  MATURE)*0.001,"\n");
      cat("#tmZtoZ_xzz: size transition matrix","\n");
      cat(prGr_xzz,"\n");
      cat("#prMatNS(nSXs,nZs): maturity curve new shell female male","\n");
      cat(modPrM2M(FEMALE),"\n");
      cat(modPrM2M(MALE),"\n");
      cat("#prMoltImm(nSXs,nZs): molting probability immature female male","\n");
      cat(prMoltImm_xz(FEMALE),"\n");
      cat(prMoltImm_xz(  MALE),"\n");
      cat("#prMoltMat(nSXs,nZs): molting probability mature female male","\n");
      cat(prMoltMat_xz(FEMALE),"\n");
      cat(prMoltMat_xz(  MALE),"\n");
      cat(0.5     ,"\t\t#recPropAsMale: proportion recruiting as males","\n");
      cat(1.0     ,"\t\t#recPropAsNewShell: prop recruits to new shell","\n");
      cat("#recPropAtZ(nZs): distribution of recruits to length bins","\n");
      cat(prRec_z,"\n");
      cat("#propEast(nZs): proportion of population at size east of 166W","\n");
      cat("???????????????????????????????","\n");

}
