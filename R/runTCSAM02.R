#'
#'@title Function to run TCSAM02.
#'
#'@description This function runs a TCSAM02 model once.
#'
#'@details
#'This function creates a shell script ('./tmp.sh') in the
#'working directory and uses it to run a version of the TCSAM02 model.\cr
#'Initial model parameters can be jittered based on the system clock time or using iSeed
#'to set the random number generator. The iSeed and final objective function value are
#'saved for each model run in a csv file (the value of out.csv).
#'
#'@param os   - 'win', 'mac', 'osx', or 'linux'
#'@param path - path for model output
#'@param model - TCSAM02 model executable name
#'@param path2model - path to model executable
#'@param configFile - filename (including path) to model configuration file
#'@param pin  - T/F to use a pin file
#'@param minPhase - phase to start minimization (or NULL)
#'@param maxPhase - final minimization phase (or NULL)
#'@param calcOFL - flag (T/F) to perform OFL calculations
#'@param calcTAC - flag (T/F) to calculate the TAC for the next fishing year
#'@param HCR - integer indicating the Harvest Control Rule used to calculate the TAC
#'@param calcDynB0 - flag to calculate dynamic B0
#'@param hess - T/F to compute hessian (and .std file)
#'@param mcmc - T/F to run mcmc
#'@param mc.N - number of mcmc iterations to do
#'@param mc.save - number of iterations to skip when saving mcmc calculations
#'@param mc.scale - number of iterations to adjust scale for mcmc calculations
#'@param jitter  - T/F to jitter parameters
#'@param iSeed - seed for random number generator (or NULL)
#'@param saveResults - T/F to save results to ModelResults.RData as a tcsam02.resLst object using \code{getResLst(...)}
#'@param test - flag (T/F) to run function in "test" mode
#'@param cleanup - flag (T/F) to clean up some output files
#'@param verbose - flag to print debugging info
#'
#'@return - dataframe of class 'tcam02.par', with 2 columns (name, value) with jitter iSeed (if jittered)
#'and par file info, or NULL if par file does not exist.
#'
#'@details If the path associated with \code{configFile} is a relative one, it should
#'be relative to the \code{path} variable. If saveResults=TRUE, getResLSt() is used to read in
#'the report file, prs file, and std files are read in and the resulting tcsam02.resLst object is
#' saved to 'ModelResults.RData'. If jitter=TRUE, hess=FALSE, and cleanup=TRUE, then most output files
#' (including the .rep files) are deleted after the model run to save disk space.
#'
#'@export
#'
runTCSAM02<-function(os='osx',
                     path='.',
                     model='tcsam02',
                     path2model='',
                     configFile='',
                     pin=FALSE,
                     pinFile=NULL,
                     mseMode=NULL,
                     minPhase=1,
                     maxPhase=NULL,
                     calcOFL=FALSE,
                     calcTAC=FALSE,
                     HCR=1,
                     calcDynB0=FALSE,
                     hess=FALSE,
                     mcmc=FALSE,
                     mc.N=1000000,
                     mc.save=1000,
                     mc.scale=1000,
                     jitter=FALSE,
                     iSeed=NULL,
                     saveResults=hess,
                     test=FALSE,
                     cleanup=TRUE,
                     verbose=FALSE){

    #start timing
    stm<-Sys.time();

    #switch to run folder (create if necessary)
    currdir<-getwd();
    on.exit(setwd(currdir));
    if (!file.exists(path)) dir.create(path,recursive=TRUE)
    setwd(path);
    cat("Running tcsam02 model at '",path,"'.\n");

    #set up copy commands
    fn.par<-file.path(getwd(),"&&model.par");
    fn.par<-gsub('&&model',tolower(model),fn.par)

    run.cmds<-getRunCommands(os=os,
                             model=model,
                             path2model=path2model,
                             configFile=configFile,
                             pin=pin,
                             pinFile=pinFile,
                             mseMode=mseMode,
                             minPhase=minPhase,
                             maxPhase=maxPhase,
                             hess=hess,
                             mcmc=mcmc,
                             mc.N=mc.N,
                             mc.save=mc.save,
                             mc.scale=mc.scale,
                             jitter=jitter,
                             iSeed=iSeed,
                             calcOFL=calcOFL,
                             calcTAC=calcTAC,
                             HCR=HCR,
                             calcDynB0=calcDynB0,
                             fullClean=jitter&(!hess)&cleanup,
                             cleanup=cleanup)
    if (tolower(os)=='win'){
        cat(run.cmds,file="tmp.bat")
        Sys.chmod("tmp.bat",mode='7777')
        if (!test) system("tmp.bat",wait=TRUE);
    } else {
        cat(run.cmds,file="./tmp.sh")
        Sys.chmod("./tmp.sh",mode='7777')
        if (!test) system("./tmp.sh",wait=TRUE);
    }

    #print timing-related info
    etm<-Sys.time();
    elt<-etm-stm;
    cat("start time: ")
    print(stm);
    cat("end time: ")
    print(etm);
    cat("elapsed time: ")
    print(elt);

    #parse par file into dataframe
    par<-paste(model,'.par',sep='')
    dfr<-NULL;
    if (!test) dfr<-rTCSAM02::readParFile(par);

    #get jitter info
    if (!test){
        if (jitter&(!is.null(dfr))) {
            tbl<-read.csv('jitterInfo.csv',header=TRUE);
            if ("B0" %in% colnames(tbl)){
                dfr<-rbind(data.frame(name='seed',value=tbl$seed[1]),
                           data.frame(name='MMB', value=tbl$MMB[1]),
                           data.frame(name='B0',  value=tbl$B0[1]),
                           data.frame(name='Bmsy',value=tbl$Bmsy[1]),
                           data.frame(name='Fmsy',value=tbl$Fmsy[1]),
                           data.frame(name='OFL', value=tbl$OFL[1]),
                           data.frame(name='curB',value=tbl$curB[1]),
                           dfr);
            } else {
                dfr<-rbind(data.frame(name='seed',value=tbl$seed[1]),
                           data.frame(name='MMB', value=tbl$MMB[1]),
                           dfr);
            }
            dfr$value[dfr$name=='objective function']<-tbl$objfun[1];
            dfr$value[dfr$name=='max gradient']<-tbl$maxGrad[1];
        }
    }#!test

    if (!test & saveResults){
        resLst<-rTCSAM02::getResLst(inp.dir="./",rep=paste0(model,".rep"),model=model,prsType='all')
        save(resLst,file="ModelResults.RData")
    }

    #return dataframe (and return to original folder as working directory)
    if (!is.null(dfr)) class(dfr)<-c('tcsam02.par',class(dfr));
    return(dfr);
}
