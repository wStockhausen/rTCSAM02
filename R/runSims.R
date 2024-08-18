#'
#'@title Function to run TCSAM02 multiple times using initial parameter values to create simulated data for fits.
#'
#'@description This functions runs a TCSAM02 model multiple times initial
#'parameter values to create simulated data to assess model consistency and estimation bias.
#'
#'@details
#'For each model run, this function creates a shell script ('./tmp.sh') in the
#'working directory and uses it to run the ADMB version of the TCSAM02 model in
#'"fit sim" mode.
#'Initial model parameters are taken from a preliminary model run and used as the basis
#'for simulating data prior to fitting the model. The seed and final objective function value are
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
#'@param pinFile - path to pin file w/ model parameters for simulation
#'@param numRuns    - number of jitter runs to make
#'@param minPhase - phase in which to start optimization
#'@param out.csv - filename for summary simulation results
#'@param calcOFL - flag (T/F) to perform OFL calculations
#'@param cleanup - T/F to clean up SOME model output files after each run
#'@param keepFiles - vector of file names to keep, not clean up, after model run
#'@param cleanupAll - T/F to clean up ALMOST ALL model output files after each run
#'
#'@return - list w/ 2 elements:
#'  dfrSmry - dataframe with seed, objective function, and other summary values from all model runs
#'  parList - list of par dataframes for each model run
#'
#'@export
#'
runSims<-function(os='osx',
                  path='.',
                  model='tcsam02',
                  path2model='',
                  configFile='',
                  pinFile=file.path(path,"tcsam02.pin"),
                  numRuns=3,
                  minPhase=1,
                  out.csv='simResults.csv',
                  calcOFL=FALSE,
                  cleanup=TRUE,
                  keepFiles=c("tmp.sh","tcsam02.par"),
                  cleanupAll=FALSE){
  #start timing
  stm<-Sys.time();

  #set up output
  out.csv<-file.path(path,out.csv)

  #run models
  rc<-0;
  parList = list();
  tblList = list();
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
                      pin=TRUE,
                      pinFile=pinFile,
                      minPhase=minPhase,
                      calcOFL=calcOFL,
                      hess=FALSE,
                      mcmc=FALSE,
                      fitSimData=TRUE,
                      simDataSeed=NULL,
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
              utils::write.table(tbl,file=out.csv,sep=",",col.names=FALSE,row.names=FALSE,append=TRUE)
          } else {
              #create out.csv file
              utils::write.table(tbl,file=out.csv,sep=",",col.names=TRUE,row.names=FALSE,append=FALSE)
          }
          tblList[[fldr]] = tbl;
      }#par not NULL
      parList[[fldr]]<-par;
      if (cleanupAll){
          cat("Cleaning up 'all' files\n\n")
          fns<-list.files(path=p2f,full.names=FALSE);#vector of file names in folder p2f
          rmf<-fns[!(fns %in% keepFiles)];           #vector of file names in nfolder p2f to remove
          file.remove(file.path(p2f,rmf));           #leave only files to keep
      }
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

  #return output
  dfrTbl = dplyr::bind_rows(tbl);
  return(list(dfrTbl=dfrTbl,parList=parList));
}
#res<-jitterTCSAM02(200);
