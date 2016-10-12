#'
#'@title Get a TCSAM2015 prs object by reading initial and final parameters csv files
#'
#'@description Function to get a TCSAM2015 prs object by reading initial and final parameters csv file.
#'
#'@param type - 'all' or 'active'
#'@param inp.dir - folder for files
#'
#'@return A prs model object (a dataframe). The returned object will be a list of class 'tcsam2015.prs'.
#'
#'@details To create the prs object, this function reads 2 csv-type parameter value files,
#'one associated with the initial parameter values and one associated with the final values.
#'The user can select to return either all the parameters, or only the active parameters (ones
#'with phase > 0). The returned object will be a dataframe of class 'tcsam2015.prs'.
#'
#'@export
#'
getPrs<-function(type='all',inp.dir='.',verbose=FALSE){
    if (verbose) cat("rTCSAM02::getPrs(): Starting...\n")
    if (!any(type==c('all','active'))) {
        cat("type = '",type,"' undefined for function rTCSAM02::getPrs(...).\n",sep='');
        cat("Returning NULL.\n\n");
        return(NULL);
    }
    if (is.null(inp.dir)) {
      inp.dir<-tcltk::tk_choose.dir(caption="Select folder with parameter CSV files");
    }
    ##get initial parameter values
    iCSV<-file.path(inp.dir,paste0("TCSAM02.params.",type,".init.csv"));
    if (file.exists(iCSV)){
        if (verbose) cat("rTCSAM02::getPrs(): Reading \n\t'",iCSV,"'.\n",sep='')
        iPRS<-readParamsCSV(iCSV);
    } else {
        cat("rTCSAM02::getPrs(): \n\t'",iCSV,"'\ndoes not exist.\n",sep='')
        cat("Returning NULL\n");
        return(NULL);
    }
    ##get final parameter values
    fCSV<-file.path(inp.dir,paste0("TCSAM02.params.",type,".final.csv"));
    if (file.exists(fCSV)){
        if (verbose) cat("rTCSAM02::getPrs(): Reading \n\t'",fCSV,"'.\n",sep='')
        fPRS<-readParamsCSV(fCSV);
    } else {
        cat("rTCSAM02::getPrs(): \n\t'",fCSV,"'\ndoes not exist.\n",sep='')
        cat("Returning NULL\n");
        return(NULL);
    }

    ##combine initial and final values
    prsObj<-cbind(fPRS,init=iPRS$value);
    prsObj<-prsObj[,c(1:6,10,7:9)];
    class(prsObj)<-c('tcsam02.prs',class(prsObj));#set class attribute to 'tcsam02.prs' for identification

    if (verbose) cat("rTCSAM02::getPrs(): Done!\n")
    return(prsObj);
}
