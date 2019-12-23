#'
#'@title Generate run commands for a tcsam02 model run
#'
#'@description Function to generate a script to make a tcsam02 model run
#'
#'@param os - 'win', 'mac', 'osx', or 'linux'
#'@param model - admb model name
#'@param path2model - path to model
#'@param configFile - filename (including path) to model configuration file
#'@param pin - flag (T/F) to use a pin file
#'@param pinFile - name of pin file (if different from default)
#'@param mseMode - mse mode for model run (NULL [off: default], "opModMode", "estModMode")
#'@param hess - flag (T/F) to calculate the hessian
#'@param minPhase - start phase (or NULL) for minimization calculations
#'@param maxPhase - last phase (or NULL) for minimization calculations
#'@param mcmc - flag (T/F) to do mcmc calculations
#'@param mc.N - number of mcmc iterations to do
#'@param mc.save - number of iterations to skip when saving mcmc calculations
#'@param mc.scale - number of iterations to adjust scale for mcmc calculations
#'@param jitter - flag (T/F) to use jitter initial values
#'@param iSeed - value for random number seed
#'@param calcOFL - flag (T/F) to do OFL calculations
#'@param calcTAC - flag (T/F) to calculate the TAC for the next fishing year
#'@param HCR - integer indicating the Harvest Control Rule used to calculate the TAC
#'@param calcDynB0 - flag (T/F) to do dynamic B0 calculations
#'@param fullClean - flag to clean up almost all files (use when making multiple jitter runs)
#'@param cleanup - flag (T/F) to clean up unnecessary files
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@details. If \code{cleanup} is TRUE, then .bar, .b0*, .p0*, .r0*, variance,
#'EchoOut.dat, CheckFile.dat, and fimn.log files are deleted.\cr
#'If the path associated with \code{configFile} is a relative one, it should
#'be relative to the path for model output.
#'
#'@export
#'
getRunCommands<-function(os='osx',
                         model='tcsam02',
                         path2model='',
                         configFile='',
                         pin=FALSE,
                         pinFile=NULL,
                         mseMode=NULL,
                         hess=FALSE,
                         minPhase=NULL,
                         maxPhase=NULL,
                         mcmc=FALSE,
                         mc.N=1000000,
                         mc.save=1000,
                         mc.scale=1000,
                         jitter=FALSE,
                         iSeed=NULL,
                         calcOFL=FALSE,
                         calcTAC=FALSE,
                         HCR=1,
                         calcDynB0=FALSE,
                         fullClean=TRUE,
                         cleanup=TRUE,
                         verbose=FALSE){
    nopath<-FALSE;
    if ((path2model=='.')||(path2model=='./')||(path2model=='.\\')||(path2model=="")) nopath=TRUE;
    echo.on <-"echo on";
    echo.off<-"echo off";
    cln<-"";
    if (cleanup) {
        cln<-"del &&model1
            del &&model.bar
            del &&model.b0*
            del &&model.p0*
            del &&model.r0*
            del variance
            del EchoData.dat
            del CheckFile.dat
            del fmin.log";
        if (fullClean){
            fullcln<-"del *.rep
                      del &&model.ModelFits.?-1.R
                      del &&model.SimData.init.dat";
            cln<-paste0(cln,"\n",fullcln);
        }
    }
    rn.mcmc<-'';
    cpy<-'';
    opts <- " -rs -nox  -configFile &&configFile &&mnPhs &&mxPhs &&mcmc &&nohess &&calcOFL &&calcTAC &&HCR &&calcDynB0 &&jitter &&iSeed &&pin !!pinFile &&mseMode";
    if (tolower(os)=='win'){
        model1<-paste(model,'exe',sep='.');
        if (!nopath) cpy<-"copy &&path2model &&model1";
        rn.mdl<-paste("&&model",opts);
        if (mcmc) rn.mcmc<-"&&model  -configFile &&configFile -mceval &&calcOFL &&calcDynB0";
        ##cln is correct for 'win', so do nothing
        run.cmds<-paste(echo.on,cpy,rn.mdl,rn.mcmc,cln,sep="\n");
        path2model<-gsub("/","\\",file.path(path2model,model1),fixed=TRUE);
    } else if (tolower(os) %in% c('mac','osx','linux')){
        model1<-model;
        if (!nopath) cpy<-"cp &&path2model ./&&model";
        rn.mdl<-paste("./&&model",opts);
        if (mcmc) rn.mcmc<-"./&&model  -configFile &&configFile -mceval &&calcOFL &&calcDynB0";
        if (cleanup) cln<-gsub("del ","rm ",cln,fixed=TRUE);
        cdr<-paste('DIR="$( cd "$( dirname "$0" )" && pwd )"','cd ${DIR}',sep='\n');
        run.cmds<-paste("#!/bin/sh",echo.on,cdr,cpy,rn.mdl,rn.mcmc,cln,sep="\n");
        path2model<-file.path(path2model,model1);
    }
    if (!nopath) run.cmds<-gsub("&&path2model",  path2model,  run.cmds,fixed=TRUE);
    run.cmds<-gsub("&&model1",      model1,      run.cmds,fixed=TRUE);
    run.cmds<-gsub("&&model",       model,       run.cmds,fixed=TRUE);
    run.cmds<-gsub("&&configFile",  configFile,  run.cmds,fixed=TRUE);
    str<-''; if (pin) str<-"-pin";
    run.cmds<-gsub("&&pin",str,run.cmds,fixed=TRUE);
    str<-''; if (!is.null(pinFile)) str<-pinFile;
    run.cmds<-gsub("!!pinFile",str,run.cmds,fixed=TRUE);
    str<-''; if (!is.null(mseMode)) str<-paste0("-mseMode ",mseMode);
    run.cmds<-gsub("&&mseMode",str,run.cmds,fixed=TRUE);
    str<-''; if (!hess) str<-"-nohess";
    run.cmds<-gsub("&&nohess",str,run.cmds,fixed=TRUE);
    str<-''; if (calcOFL) str<-"-calcOFL";
    run.cmds<-gsub("&&calcOFL",str,run.cmds,fixed=TRUE);
    str<-''; if (calcTAC) str<-"-calcTAC";
    run.cmds<-gsub("&&calcTAC",str,run.cmds,fixed=TRUE);
    str<-''; if (calcTAC) str<-paste0(HCR);
    run.cmds<-gsub("&&HCR",str,run.cmds,fixed=TRUE);
    str<-''; if (calcDynB0) str<-"-calcDynB0";
    run.cmds<-gsub("&&calcDynB0",str,run.cmds,fixed=TRUE);
    str<-''; if (jitter) str<-"-jitter";
    run.cmds<-gsub("&&jitter",str,run.cmds,fixed=TRUE);
    str<-''; if (is.numeric(iSeed)) str<-paste("-iSeed",iSeed);
    run.cmds<-gsub("&&iSeed",str,run.cmds,fixed=TRUE);
    str<-''; if (mcmc) str<-paste("-mcmc",mc.N,"-mcsave",mc.save,"-mcscale",mc.scale);
    run.cmds<-gsub("&&mcmc",str,run.cmds,fixed=TRUE);
    str<-''; if (!is.null(minPhase)) str<-paste0("-phase ",minPhase);
    run.cmds<-gsub("&&mnPhs",str,run.cmds,fixed=TRUE);
    str<-''; if (!is.null(maxPhase)) str<-paste0("-maxph ",maxPhase);
    run.cmds<-gsub("&&mxPhs",str,run.cmds,fixed=TRUE);

    if (verbose) cat("Run commands:\n",run.cmds,"\n\n");

    return(run.cmds);
}
