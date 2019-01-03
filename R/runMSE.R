#'
#'@title Function to run the TCSAM02 MSE for one harvest strategy
#'
#'@description This functions runs the TCSAM02 MSE for one harvest strategy.
#'
#'@details TODO!
#'
#'@param os   - 'win', 'mac', 'osx', or 'linux'
#'@param topLevelFolder - path to top-level folder for MSE model runs
#'@param model      - TCSAM02 model executable name
#'@param path2model - path to model executable
#'@param configFile - path to model configuration file
#'@param HCR - integer identifying harvest control rule to use
#'@param numRuns    - number of complete MSE runs to make
#'@param minRunID   - integer identifying first run label for the series of runs (default=1)
#'@param firstYr    - first year for projection (i.e., the assessment year, or max year + 1, of base model)
#'@param numYrs     - number of years to run model forward
#'@param runBaseModel - flag to run base model to calculate TAC and create operating model state for first projection year
#'@param baseModelInfo - list of parameters to run base model (if necessary)
#'@param opModInfo - list of parameters to run operating model
#'@param estModInfo - list of parameters to run estimation model
#'@param keepFiles  - vector of file names to keep after model run, if cleanupAll is true
#'@param cleanupAll - T/F to clean up ALMOST ALL model output files after each run
#'@param test - flag (T/F) to run in "test" mode
#'@param verbose    - flag (T/F) to print diagnostic messages
#'
#'@return - NULL
#'
#'@export
#'
runMSE<-function(os='osx',
                 topLevelFolder='.',
                 model='tcsam02',
                 path2model='',
                 HCR=1,
                 numRuns=3,
                 minRunID=1,
                 firstYr=2018,
                 numYrs=10,
                 runBaseModel=TRUE,
                 baseModelInfo=list(path=".",
                                    configFile="ModelConfig.inp",
                                    datasets=list(file="Datasets.inp",
                                                  components=list(
                                                      Bio="Info",
                                                      Fishery=c("TCF","SCF","GTF","RKF"),
                                                      Survey="NMFS",
                                                      Growth="EBS",
                                                      ChelaHeights=NULL)),
                                    pin=TRUE,
                                    pinFile="tcsam02.pin",
                                    minPhase=1,
                                    jitter=FALSE,
                                    calcTAC=TRUE,
                                    HCR=1),
                 opModInfo=list(path=".",
                                configFile="OpMod.Configuration.inp",
                                optsFile="OpMod.Options.inp",
                                mpiFile="OpMod.ParametersInfo,inp",
                                pinFile="opMod.pin",
                                minPhase=5,
                                maxPhase=6),
                 estModInfo=list(path=".",
                                 configFile="EstMod.Configuration.inp",
                                 optsFile="EstMod.Options.inp",
                                 minPhase=5),
                 keepFiles=c("tmp.sh","tcsam02.par"),
                 cleanupAll=FALSE,
                 test=TRUE,
                 verbose=TRUE){
    #start timing
    stm<-Sys.time();

    #define helper function
    myFileCopy<-function(from,to,overwrite=TRUE,fromEQtoOK=FALSE){
        if (!file.exists(from)) stop(paste0("#--ERROR: 'from' file does not exist\n\t",from,"\n"));
        if ((!fromEQtoOK)&(from==to)) stop(paste0("#--ERROR: 'from' and 'to' are same file!:\n",from));
        res<-file.copy(from,to,overwrite=overwrite);
        if (!res) stop(paste0("#--ERROR copying\n\t",from,"to\n\t",to,"\n"));
    }

    #Determine input dataset filenames
    dataFileNames<- baseModelInfo$datasets$file;
    comps<-names(baseModelInfo$datasets$components);
    for (comp in comps){
        for (nm in baseModelInfo$datasets$components[[comp]]){
            dataFileNames<-c(dataFileNames,paste("Data",comp,nm,"inp",sep="."));
        }
    }

    #--move to topLevelFolder
    if (!dir.exists(topLevelFolder)) dir.create(topLevelFolder,recursive=TRUE);
    currDir<-setwd(topLevelFolder);
    on.exit(setwd(currDir));
    topLevelFolder<-normalizePath(getwd());
    cat("#--Top level folder is \n\t'",topLevelFolder,"'\n",sep="");

    #--define folder for OpMod files
    opModFilesFolder<-file.path(topLevelFolder,"OpModFiles");
    cat("#--OpMod files folder:",opModFilesFolder,sep="\n\t");
    #--define OpMod file names
    opModConfigFile <-file.path(opModFilesFolder,"OpMod.Configuration.inp");
    opModOptsFile   <-file.path(opModFilesFolder,opModInfo$optsFile);
    opModMPIFile    <-file.path(opModFilesFolder,opModInfo$mpiFile);
    opModPinFile    <-file.path(opModFilesFolder,"OpModPinFile.pin");

    #--define folder for EstMod files
    estModFilesFolder<-file.path(topLevelFolder,"EstModFiles");
    cat("#--EstMod files folder:",estModFilesFolder,sep="\n\t");
    #define EstMod file names
    estModConfigFile<-file.path(estModFilesFolder,"EstMod.Configuration.inp");
    estModOptsFile  <-file.path(estModFilesFolder,"EstMod.Options.inp");
    estModMPIFile   <-file.path(estModFilesFolder,"EstMod.ParametersInfo.inp");
    estModPinFile   <-file.path(estModFilesFolder,"EstModPinFile.pin");

    if (runBaseModel){
        #--define and create folder for base model files
        baseFilesFolder<-file.path(topLevelFolder,"BaseModelFiles");
        if (!dir.exists(baseFilesFolder)) dir.create(baseFilesFolder,recursive=TRUE);
        #--copy input files from model config folder to baseFilesFolder
        modelConfigFolder<-normalizePath(file.path(currDir,baseModelInfo$path));
        cat("--base model config folder is '",modelConfigFolder,"'\n",sep="");
        fs<-list.files(path=modelConfigFolder,pattern=glob2rx("*.inp"),full.names=FALSE);
        for (f in fs) file.copy(from=file.path(modelConfigFolder,f),
                                to=file.path(baseFilesFolder,f),overwrite=TRUE);
        #--copy pin file to baseFilesFolder
        if (baseModelInfo$pin) file.copy(from=file.path(modelConfigFolder,baseModelInfo$pinFile),
                                         to=file.path(baseFilesFolder,baseModelInfo$pinFile));
        #--identify (and create, if necessary) folder for base model run
        baseRunFolder<-file.path(topLevelFolder,paste0("BaseModelRun",firstYr));
        if (!dir.exists(baseRunFolder)) dir.create(baseRunFolder,recursive=FALSE);
        cat("#--Base model run folder is \n\t'",baseRunFolder,"'\n",sep="");
        #--copy all files from baseFilesFolder to baseRunFolder
        fs<-list.files(path=baseFilesFolder,pattern=glob2rx("*.*"),full.names=FALSE);
        for (f in fs) file.copy(from=file.path(baseFilesFolder,f),
                                to=file.path(baseRunFolder,f),overwrite=TRUE);

        #--run TCSAM02 to create base model
        cat("#--Running base model.\n");
        runTCSAM02(os=os,
                   path=baseRunFolder,
                   configFile=baseModelInfo$configFile,
                   model=model,
                   path2model=path2model,
                   pin=ifelse(!is.null(baseModelInfo$pin),
                              baseModelInfo$pin,FALSE),
                   pinFile=ifelse(!is.null(baseModelInfo$pinFile),
                                  baseModelInfo$pinFile,
                                  "tcsam02.pin"),
                   mseMode=NULL,
                   minPhase=baseModelInfo$minPhase,
                   maxPhase=baseModelInfo$maxPhase,
                   calcOFL=ifelse(!is.null(baseModelInfo$OFL),
                                  baseModelInfo$OFL,TRUE),
                   calcTAC=ifelse(!is.null(baseModelInfo$calcTAC),
                                  baseModelInfo$calcTAC,TRUE),
                   HCR=ifelse(!is.null(baseModelInfo$HCR),
                              baseModelInfo$HCR,1),
                   calcDynB0=ifelse(!is.null(baseModelInfo$calcDynB0),
                                    baseModelInfo$calcDynB0,FALSE),
                   jitter=ifelse(!is.null(baseModelInfo$jitter),
                                 baseModelInfo$jitter,FALSE),
                   jit.seed=ifelse(!is.null(baseModelInfo$jit.seed),
                                   baseModelInfo$jit.seed,-1),
                   saveResults=ifelse(!is.null(baseModelInfo$saveResults),
                                      baseModelInfo$cleanup,FALSE),
                   test=ifelse(!is.null(baseModelInfo$test),
                               baseModelInfo$test,FALSE),
                   cleanup=ifelse(!is.null(baseModelInfo$jitter),
                                  baseModelInfo$cleanup,FALSE),
                   verbose=verbose);

        #--define and create folder for OpMod files
        if (!dir.exists(opModFilesFolder)) dir.create(opModFilesFolder,recursive=TRUE);
        cat("#--Created OpMod files folder:",opModFilesFolder,sep="\n\t");
        #--copy required files to OpModFilesFolder
        #----TAC and OpModState files from base files folder
        baseTACFile<-list.files(baseRunFolder,pattern=glob2rx("TAC*.txt"),full.names=TRUE);
        baseOMSFile<-list.files(baseRunFolder,pattern=glob2rx("OpModStateFile*.txt"),full.names=TRUE);
        cat("#--Copying base TAC file:\n\t",baseTACFile,"\n");
        cat("#--Copying base OMS file:\n\t",baseOMSFile,"\n");
        myFileCopy(baseTACFile,file.path(opModFilesFolder,"TAC.txt"));
        myFileCopy(baseOMSFile,file.path(opModFilesFolder,"OpModStateFile.txt"));
        #----dataset files from base model files folder
        for (f in dataFileNames) myFileCopy(file.path(baseFilesFolder,f), file.path(opModFilesFolder,f));
        #----other files from input operating model files folder
        myFileCopy(file.path(currDir,opModInfo$path,opModInfo$configFile),opModConfigFile)
        myFileCopy(file.path(currDir,opModInfo$path,opModInfo$optsFile),  opModOptsFile)
        myFileCopy(file.path(currDir,opModInfo$path,opModInfo$mpiFile),   opModMPIFile);
        myFileCopy(file.path(currDir,opModInfo$path,opModInfo$pinFile),   opModPinFile);
        cat("#--Copied base OpMod congfig file:\n\t",opModConfigFile,"\n");
        cat("#--Copied base OpMod options file:\n\t",opModOptsFile,"\n");
        cat("#--Copied base OpMod MPI file:    \n\t",opModMPIFile,"\n");
        cat("#--Copied base OpMod pin file:    \n\t",opModPinFile,"\n");

        #--define and create folder for EstMod files
        if (!dir.exists(estModFilesFolder)) dir.create(estModFilesFolder,recursive=TRUE);
        cat("#--Creating EstMod files folder:",estModFilesFolder,sep="\n\t");
        #--copy EstMod Config and Options file from the input estimation model files folder
        myFileCopy(file.path(currDir,estModInfo$path,estModInfo$configFile),estModConfigFile);
        myFileCopy(file.path(currDir,estModInfo$path,estModInfo$optsFile),  estModOptsFile);
        #--copy EstMod MPI and Pin files from the base run folder
        myFileCopy(file.path(baseRunFolder,paste0("EstMod.ParametersInfo.",firstYr,".inp")), estModMPIFile);
        myFileCopy(file.path(baseRunFolder,paste0("EstModPinFile_",firstYr,".txt")),         estModPinFile);
        cat("#--Copied base EstMod congfig file:\n\t",estModConfigFile,"\n");
        cat("#--Copied base EstMod options file:\n\t",estModOptsFile,"\n");
        cat("#--Copied base EstMod MPI file:    \n\t",estModMPIFile,"\n");
        cat("#--Copied base EstMod pin file:    \n\t",estModPinFile,"\n");
        cat("\n\n")
    } else {
        cat("#--Skipping base model run.\n");
        cat("#--REQUIRED FILES to start MSE should be in the folders\n\t",opModFilesFolder,"\n\t",estModFilesFolder,"\n");
        cat("\n\n")
    }

    #--check base folder for OpMod files
    if (!dir.exists(opModFilesFolder)) stop(paste("OpModFiles folder\n\t",opModFilesFolder,"\nMUST EXIST! Aborting...\n"));
    cat("#--OpMod files folder:",opModFilesFolder,sep="\n\t");
    opModFiles<-list.files(opModFilesFolder,pattern="*.*");
    cat("#----OpMod files in folder:",opModFiles,sep="\n\t")

    #--check folder for EstMod files
    if (!dir.exists(estModFilesFolder)) stop(paste("EstModFiles folder\n\t",estModFilesFolder,"\nMUST EXIST! Aborting...\n"));
    cat("#--EstMod files folder:",estModFilesFolder,sep="\n\t");
    estModFiles<-list.files(estModFilesFolder,pattern="*.*");

    #make MSE runs
    cat("\n\n#--Starting MSE runs\n")
    runIDs <- (minRunID-1)+(1:numRuns);
    for (r in runIDs){
        cat("#----Starting MSE run",r,"\n")
        #move to top-level folder
        setwd(topLevelFolder);
        #--make folder for MSE run and copy base files
        runFolder<-paste('run',wtsUtilities::formatZeros(r,width=max(3,ceiling(log10(max(runIDs))))),sep='');
        runFolder<-file.path(topLevelFolder,runFolder);
        cat("#------runFolder will be\n\t",runFolder,"\n");
        if (!dir.exists(runFolder)) dir.create(runFolder,recursive=TRUE);

        for (y in (firstYr-1+(1:numYrs))){
            cat("\n\n#----Running TCSAM MSE for year",y,paste0("(max",firstYr-1+numYrs,") in run"),r,"out of",max(runIDs),"---\n");

            #--define and create OpMod working folder
            opfldr<-paste0(wtsUtilities::formatZeros(y,width=4),".OpMod");
            opModRunFolder<-file.path(runFolder,opfldr);
            cat("#------opModRunFolder will be\n\t",opModRunFolder,"\n");
            if (!dir.exists(opModRunFolder)) dir.create(opModRunFolder,recursive=TRUE);
            #----copy required files into working folder
            #------copy ALL files from opModFilesFolder
            for (f in opModFiles) myFileCopy(file.path(opModFilesFolder,f),file.path(opModRunFolder,f));
            #------update newly-copied config file in opModRunFolder for year "y"
            tmpCFG<-file.path(opModRunFolder,basename(opModConfigFile));
            str <- readLines(tmpCFG);
            str<-gsub("&&year",y,str);#assessment year in OpModMode is y
            writeLines(str,tmpCFG); rm(str,tmpCFG);
            #------identify state and TAC files from "last" year (already in opModRunFolder )
            opModStateFile<-file.path(opModRunFolder,"OpModStateFile.txt");
            opModTACFile  <-file.path(opModRunFolder,"TAC.txt");
            if (y==firstYr){
                #--Do nothing: already copied the correct files
            } else {
                #--copy OpMod state from previous year to OpModRunFolder
                oldOpModRunFolder<-paste(wtsUtilities::formatZeros(y-1,width=4),".OpMod",sep='');
                oldOpModStateFile<-file.path(runFolder,oldOpModRunFolder,paste0("OpModStateFile_",y,".txt"));
                myFileCopy(oldOpModStateFile,opModStateFile);
                #--copy TAC from EstMod folder for previous year to OpModRunFolder
                #opModTACFile  <-file.path(opModRunFolder,"TAC.txt");#<-TODO: FOR TESTING ONLY. COMMENT THIS LINE OUT!!
                estModTACFile<-file.path(estModRunFolder,paste0("TAC_",y,".txt"));
                myFileCopy(estModTACFile,opModTACFile);#<-TODO: FOR PRODUCTION. UNCOMMENT THIS LINE!!
                #copy dataset files from previous EstMod run to OpModRunFolder
                for (f in dataFileNames) myFileCopy(file.path(estModRunFolder,f), file.path(opModRunFolder,f));
            }

            #create EstModRunFolder for "next" year for OpMod output
            estfldr<-paste0(wtsUtilities::formatZeros(y+1,width=4),'.EstMod');
            estModRunFolder<-file.path(runFolder,estfldr);#--path to working folder
            cat("#----estModRunFolder will be\n\t",estModRunFolder,"\n");
            if (!dir.exists(estModRunFolder)) dir.create(estModRunFolder,recursive=TRUE);
            #copy existing files into folder
            myFileCopy(estModConfigFile, file.path(estModRunFolder,basename(estModConfigFile)));
            myFileCopy(estModOptsFile,   file.path(estModRunFolder,basename(estModOptsFile)));
            if (y==firstYr){
                #copy MPI and pin files from estModFilesFolder
                myFileCopy(estModMPIFile, file.path(estModRunFolder,basename(estModMPIFile)));
                myFileCopy(estModPinFile, file.path(estModRunFolder,basename(estModPinFile)));
            } else {
                #copy MPI and pin files from estModRunFolder for previous year
                prvEstModRunFolder<-file.path(runFolder,paste(wtsUtilities::formatZeros(y,width=4),'.EstMod',sep=''))
                myFileCopy(file.path(prvEstModRunFolder,paste0("EstMod.ParametersInfo.",y,".inp")), file.path(estModRunFolder,basename(estModMPIFile)));
                myFileCopy(file.path(prvEstModRunFolder,paste0("EstModPinFile_",y,".txt")),         file.path(estModRunFolder,basename(estModPinFile)));
            }
            #--update config file for year "y+1"
            tmpCFG<-file.path(estModRunFolder,basename(estModConfigFile));
            str <- readLines(tmpCFG);
            str<-gsub("&&year",y+1,str);#assessment year in EstModMode is y+1
            writeLines(str,tmpCFG); rm(str,tmpCFG);

            #enter OpMod folder and run operating model with current TAC
            cat("#--------Running OpModMode for year",y,"of run",r,"out of",max(runIDs),"---\n");
            setwd(opModRunFolder)
            par<-runTCSAM02(path=".",
                            os=os,
                            model=model,
                            path2model=path2model,
                            configFile=basename(opModConfigFile),
                            pin=TRUE,
                            pinFile=basename(opModPinFile),
                            mseMode="mseOpModMode",
                            minPhase=opModInfo$minPhase,
                            maxPhase=opModInfo$maxPhase,
                            calcOFL=FALSE,
                            calcDynB0=FALSE,
                            hess=FALSE,
                            mcmc=FALSE,
                            jitter=FALSE,
                            jit.seed=NULL,
                            cleanup=FALSE,
                            test=FALSE,
                            saveResults=FALSE);
            #copy data files for EstMod run to estModRunFolder folder
            if (!test&cleanupAll){
                p2f<-opModRunFolder;
                cat("Cleaning up 'all' files\n\n")
                fns<-list.files(path=p2f,full.names=FALSE);#vector of file names in folder
                rmf<-fns[!(fns %in% keepFiles)];           #vector of file names in folder to remove
                file.remove(file.path(p2f,rmf));           #leave only files to keep
            }#--cleanupAll

            cat("#--------running EstModMode for year",y+1,"of run",r,"out of",max(runIDs),"---\n\n");
            setwd(estModRunFolder)
            if (!test)
            par<-runTCSAM02(path=".",
                            os=os,
                            model=model,
                            path2model=path2model,
                            configFile=basename(estModConfigFile),
                            pin=TRUE,
                            pinFile=basename(estModPinFile),
                            mseMode="mseEstModMode",
                            minPhase=estModInfo$minPhase,
                            maxPhase=estModInfo$maxPhase,
                            calcOFL=TRUE,
                            calcTAC=TRUE,
                            HCR=HCR,
                            calcDynB0=FALSE,
                            hess=FALSE,
                            mcmc=FALSE,
                            jitter=FALSE,
                            jit.seed=NULL,
                            cleanup=FALSE,
                            test=test,
                            saveResults=FALSE);
            if (!test&cleanupAll){
                p2f<-estModRunFolder;
                cat("Cleaning up 'all' files\n\n")
                fns<-list.files(path=p2f,full.names=FALSE);#vector of file names in folder p2f
                rmf<-fns[!(fns %in% keepFiles)];           #vector of file names in nfolder p2f to remove
                file.remove(file.path(p2f,rmf));           #leave only files to keep
            }#--cleanupAll
        }#--y
    }#--r

    #print timing-related info
    cat("\n\n#--MSE timing information--\n");
    etm<-Sys.time();
    elt<-etm-stm;
    cat("start time: ")
    print(stm);
    cat("end time: ")
    print(etm);
    cat("elapsed time: ")
    print(elt);
    cat("#------------------------------\n");

    return(NULL);
}

