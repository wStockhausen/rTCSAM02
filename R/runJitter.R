#'
#'@title Function to run TCSAM02 multiple times using jittered initial parameter values.
#'
#'@description This functions runs a TCSAM02 model multiple times, jittering the
#'initial staarting values to assess model convergence.
#'
#'@details
#'For each model run, this function creates a shell script ('./tmp.sh') in the
#'working directory and uses it to run the ADMB version of the TCSAM02 model.
#'Initial model parameters are jittered based on the system clock time as a seed
#'to the random number generator. The seed and final objective function value are
#'saved for each model run in a csv file (the value of out.csv).
#'
#'When all the models requested have been run,
#'the function determines the seed associated with the 1st model run that yielded
#'the smallest value for the objective function and re-runs the model using this seed
#'to re-create the model run resulting in the minimum objectve function to recreate
#'the model output files. The final model run is done estimating the hessian, so
#'standard deviations for estimated model parameters are available in the .std file.
#'
#'@param os   - 'win', 'mac', 'osx', or 'linux'
#'@param path - path for model output
#'@param model      - TCSAM02 model executable name
#'@param path2model - path to model executable
#'@param configFile - path to model configuration file
#'@param numRuns    - number of jitter runs to make
#'@param minPhase - phase in which to start optimization
#'@param onlyEvalJitter - flag (T/F) to only evaluate a (previous) set of jitter runs, not make new runs
#'@param in.csv - filename for jitter info (seed, obj fun value) from ADMB model run
#'@param out.csv - filename for jittered results
#'@param calcOFL - flag (T/F) to perform OFL calculations for "best" model
#'@param calcOFLJitter - flag (T/F) to perform OFL calculations while jittering
#'@param mcmc - flag (T/F) to run mcmc on "best" model
#'@param mc.N - number of mcmc iterations to make
#'@param mc.save - number of iterations to skip when saving mcmc calculations
#'@param mc.scale - number of iterations to adjust scale for mcmc calculations
#'@param saveResults - T/F to save final results to best/ModelResults.RData
#'@param cleanup - T/F to clean up SOME model output files after each run
#'@param keepFiles - vector of file names to keep, not clean up, after model run
#'@param cleanupAll - T/F to clean up ALMOST ALL model output files after each run
#'
#'@return - list w/ 4 elements:
#'  imx  - index of (1st) smallest value for the objective function
#'  seed - seed resulting in the smallest objective function
#'  par  - dataframe with par results from run w/ smallest objective function
#'  objFuns - vector of objective function values from all model runs
#'  parList - list of par dataframes for each model run
#'
#'@export
#'
runJitter<-function(os='osx',
                    path='.',
                    model='tcsam02',
                    path2model='',
                    configFile='',
                    numRuns=3,
                    minPhase=1,
                    onlyEvalJitter=FALSE,
                    in.csv='jitterInfo.csv',
                    out.csv='jitterResults.csv',
                    calcOFL=FALSE,
                    calcOFLJitter=FALSE,
                    mcmc=FALSE,
                    mc.N=1000000,
                    mc.save=1000,
                    mc.scale=1000,
                    saveResults=FALSE,
                    cleanup=TRUE,
                    keepFiles=c("tmp.sh","tcsam02.par"),
                    cleanupAll=FALSE){
    #start timing
    stm<-Sys.time();

    #set up output
    out.csv<-file.path(path,out.csv)

    #run models
    if (!onlyEvalJitter){
        rc<-0;
        parList<-list();
        for (r in 1:numRuns){
            cat("\n\n---running ADMB program for",r,"out of",numRuns,"---\n\n");
            fldr<-paste('run',wtsUtilities::formatZeros(r,width=max(2,ceiling(log10(numRuns)))),sep='');
            p2f<-file.path(path,fldr);
            par<-runTCSAM02(path=p2f,
                            os=os,
                            model=model,
                            test=FALSE,
                            path2model=path2model,
                            configFile=configFile,
                            pin=FALSE,
                            minPhase=minPhase,
                            calcOFL=calcOFLJitter,
                            hess=FALSE,
                            mcmc=FALSE,
                            jitter=TRUE,
                            jit.seed=NULL,
                            cleanup=cleanup,
                            saveResults=FALSE);
            if (!is.null(par)){
                rc<-rc+1;
                objFun  <-par$value[par$name=='objective function'];
                seed    <-par$value[par$name=='seed'];
                maxgrad <-par$value[par$name=='max gradient'];
                MMB     <-par$value[par$name=='MMB'];
                if ("B0" %in% par$name){
                    B0   <- par$value[par$name=='B0'];
                    Bmsy <- par$value[par$name=='Bmsy'];
                    Fmsy <- par$value[par$name=='Fmsy'];
                    OFL  <- par$value[par$name=='OFL'];
                    curB <- par$value[par$name=='curB'];
                    tbl<-data.frame(idx=r,objFun=objFun,maxGrad=maxgrad,seed=seed,MMB=MMB,
                                    B0=B0,Bmsy=Bmsy,Fmsy=Fmsy,OFL=OFL,curB=curB);
                } else {
                    tbl<-data.frame(idx=r,objFun=objFun,maxGrad=maxgrad,seed=seed,MMB=MMB);
                }
                if (file.exists(out.csv)) {
                    write.table(tbl,file=out.csv,sep=",",col.names=FALSE,row.names=FALSE,append=TRUE)
                } else {
                    #create out.csv file
                    write.table(tbl,file=out.csv,sep=",",col.names=TRUE,row.names=FALSE,append=FALSE)
                }
            }#par not NULL
            parList[[fldr]]<-par;
            if (cleanupAll){
                cat("Cleaning up 'all' files\n\n")
                fns<-list.files(path=p2f,full.names=FALSE);#vector of file names in folder p2f
                rmf<-fns[!(fns %in% keepFiles)];           #vector of file names in nfolder p2f to remove
                file.remove(file.path(p2f,rmf));           #leave only files to keep
            }
        }
    }

    #determine row index associated w/ minimum obj fun value
    #read jitter results from file
    if (file.exists(out.csv)){
        tbl<-read.csv(out.csv);
        idx<-order(tbl$objFun,abs(tbl$maxGrad));
        best<-tbl$idx[idx[1]];
        seed<-tbl$seed[idx[1]];
        if (onlyEvalJitter){parList<-NULL;}

        #re-run case associated with mininum objective function value, save in "best"
        cat("\n\n---Re-running ADMB program for",idx[1],"out of",numRuns,"as best run---\n");
        ##fldr<-paste('best.run',wtsUtilities::formatZeros(best,width=max(2,ceiling(log10(numRuns)))),sep='');
        fldr<-"best";
        p2f<-file.path(path,fldr);
        cat("---Output folder is '",p2f,"'\n\n",sep='');
        par<-runTCSAM02(path=p2f,
                        os=os,
                        model=model,
                        test=FALSE,
                        path2model=path2model,
                        configFile=configFile,
                        pin=FALSE,
                        minPhase=minPhase,
                        calcOFL=TRUE,
                        hess=TRUE,
                        mcmc=mcmc,
                        mc.N=mc.N,
                        mc.save=mc.save,
                        mc.scale=mc.scale,
                        jitter=TRUE,
                        jit.seed=seed,
                        cleanup=FALSE,
                        saveResults=saveResults);

        #print timing-related info
        etm<-Sys.time();
        elt<-etm-stm;
        cat("start time: ")
        print(stm);
        cat("end time: ")
        print(etm);
        cat("elapsed time: ")
        print(elt);

        #return output
        return(list(imn=best,seed=seed,par=par,objFuns=tbl,parList=parList));
    } else {
        #print timing-related info
        etm<-Sys.time();
        elt<-etm-stm;
        cat("start time: ")
        print(stm);
        cat("end time: ")
        print(etm);
        cat("elapsed time: ")
        print(elt);

        return(NULL);
    }
}
#res<-jitterTCSAM02(200);
