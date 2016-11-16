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
#'@param hess - flag (T/F) to calculate the hessian
#'@param minPhase - start phase (or NULL) for minimization calculations
#'@param maxPhase - last phase (or NULL) for minimization calculations
#'@param mcmc - flag (T/F) to do mcmc calculations
#'@param mc.N - number of mcmc iterations to do
#'@param mc.save - number of iterations to skip when saving mcmc calculations
#'@param mc.scale - number of iterations to adjust scale for mcmc calculations
#'@param jitter - flag (T/F) to use jitter initial values
#'@param jit.seed - value for random number seed to generate jitter
#'@param calcOFL - flag (T/F) to do OFL calculations
#'@param cleanup - flag (T/F) to clean up unnecessary files
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
                         hess=FALSE,
                         minPhase=NULL,
                         maxPhase=NULL,
                         mcmc=FALSE,
                         mc.N=1000000,
                         mc.save=1000,
                         mc.scale=1000,
                         jitter=FALSE,
                         jit.seed=NULL,
                         calcOFL=FALSE,
                         cleanup=TRUE){
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
    }
    rn.mcmc<-'';
    cpy<-'';
    if (tolower(os)=='win'){
        model1<-paste(model,'exe',sep='.');
        if (!nopath) cpy<-"copy &&path2model &&model1";
        rn.mdl<-"&&model -rs -nox  -configFile &&configFile &&mnPhs &&mxPhs &&mcmc &&nohess &&calcOFL &&jitter &&jit.seed &&pin";
        if (mcmc) rn.mcmc<-"&&model  -configFile &&configFile -mceval &&calcOFL";
        ##cln is correct for 'win', so do nothing
        run.cmds<-paste(echo.on,cpy,rn.mdl,rn.mcmc,cln,sep="\n");
        path2model<-gsub("/","\\",file.path(path2model,model1),fixed=TRUE);
    } else if (tolower(os) %in% c('mac','osx','linux')){
        model1<-model;
        if (!nopath) cpy<-"cp &&path2model ./&&model";
        rn.mdl<-"./&&model -rs -nox  -configFile &&configFile &&mnPhs &&mxPhs &&mcmc &&nohess &&calcOFL &&jitter &&jit.seed &&pin";
        if (mcmc) rn.mcmc<-"./&&model  -configFile &&configFile -mceval &&calcOFL";
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
    str<-''; if (!hess) str<-"-nohess";
    run.cmds<-gsub("&&nohess",str,run.cmds,fixed=TRUE);
    str<-''; if (calcOFL) str<-"-calcOFL";
    run.cmds<-gsub("&&calcOFL",str,run.cmds,fixed=TRUE);
    str<-''; if (jitter) str<-"-jitter";
    run.cmds<-gsub("&&jitter",str,run.cmds,fixed=TRUE);
    str<-''; if (is.numeric(jit.seed)) str<-paste("-iSeed",jit.seed);
    run.cmds<-gsub("&&jit.seed",str,run.cmds,fixed=TRUE);
    str<-''; if (mcmc) str<-paste("-mcmc",mc.N,"-mcsave",mc.save,"-mcscale",mc.scale);
    run.cmds<-gsub("&&mcmc",str,run.cmds,fixed=TRUE);
    str<-''; if (!is.null(minPhase)) str<-paste0("-phase ",minPhase);
    run.cmds<-gsub("&&mnPhs",str,run.cmds,fixed=TRUE);
    str<-''; if (!is.null(maxPhase)) str<-paste0("-maxph ",maxPhase);
    run.cmds<-gsub("&&mxPhs",str,run.cmds,fixed=TRUE);

    cat("Run commands:\n",run.cmds,"\n\n");

    return(run.cmds);
}
