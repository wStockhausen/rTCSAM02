#'
#'@title Function to run TCSAM02.
#'
#'@description This function runs a TCSAM02 model once.
#'
#'@details
#'This function creates a shell script ('./tmp.sh') in the
#'working directory and uses it to run a version of the TCSAM02 model.\cr
#'Initial model parameters can be jittered based on the system clock time as a jit.seed
#'to the random number generator. The jit.seed and final objective function value are
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
#'@param hess - T/F to compute hessian (and .std file)
#'@param mcmc - T/F to run mcmc
#'@param mc.N - number of mcmc iterations to do
#'@param mc.save - number of iterations to skip when saving mcmc calculations
#'@param mc.scale - number of iterations to adjust scale for mcmc calculations
#'@param jitter  - T/F to jitter parameters
#'@param jit.seed - seed for random number generator (or NULL)
#'@param saveResults - T/F to save results to ModelResults.RData as a tcsam02.resLst object using \code{getResLst(...)}
#'@param cleanup - flag (T/F) to clean up some output files
#'
#'@return - dataframe of class 'tcam02.par', with 2 columns (name, value) with jitter jit.seed (if jittered)
#'and par file info, or NULL if par file does not exist.
#'
#'@details If the path associated with \code{configFile} is a relative one, it should
#'be relative to the \code{path} variable. If saveResults=TRUE, getResLSt() is used to read in
#'the report file, prs file, and std files are read in and the resulting tcsam02.resLst object is
#' saved to 'ModelResults.RData'.
#'
#'@export
#'
runTCSAM02<-function(os='osx',
                     path='.',
                     model='tcsam02',
                     path2model='',
                     configFile='',
                     pin=FALSE,
                     minPhase=1,
                     maxPhase=NULL,
                     calcOFL=FALSE,
                     hess=FALSE,
                     mcmc=FALSE,
                     mc.N=1000000,
                     mc.save=1000,
                     mc.scale=1000,
                     jitter=FALSE,
                     jit.seed=NULL,
                     saveResults=hess,
                     cleanup=TRUE){
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
                             minPhase=minPhase,
                             maxPhase=maxPhase,
                             hess=hess,
                             mcmc=mcmc,
                             mc.N=mc.N,
                             mc.save=mc.save,
                             mc.scale=mc.scale,
                             jitter=jitter,
                             jit.seed=jit.seed,
                             calcOFL=calcOFL,
                             cleanup=cleanup)
    if (tolower(os)=='win'){
        cat(run.cmds,file="tmp.bat")
        Sys.chmod("tmp.bat",mode='7777')
        system("tmp.bat",wait=TRUE);
    } else {
        cat(run.cmds,file="./tmp.sh")
        Sys.chmod("./tmp.sh",mode='7777')
        system("./tmp.sh",wait=TRUE);
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
    dfr<-readParFile(par);

    #get jitter info
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

    if (saveResults){
        resLst<-getResLst(inp.dir="./",rep=paste0(model,".rep"),model=model,prsType='all')
        save(resLst,file="ModelResults.RData")
        # plotTCSAM2015I(repObj=repObj,
        #                prsObj=prsObj,
        #                stdObj=stdObj,
        #                ggtheme=theme_grey(),
        #                showPlot=TRUE,
        #                pdf="TCSAM2015.pdf",
        #                width=14,height=8)
    }

    #return dataframe (and return to original folder as working directory)
    if (!is.null(dfr)) class(dfr)<-c('tcsam02.par',class(dfr));
    return(dfr);
}
