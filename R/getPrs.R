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
getPrs<-function(type='all',inp.dir='.'){
    if (!any(type==c('all','active'))) {
        cat("type = '",type,"' undefined for function rTCSAM2015::getPrs(...).\n",sep='');
        cat("Returning NULL.\n\n");
        return(NULL);
    }
    if (is.null(inp.dir)) {
      inp.dir<-tcltk::tk_choose.dir(caption="Select folder with parameter CSV files");
    }
    ##get initial parameter values
    iCSV<-file.path(inp.dir,paste0("TCSAM02.params.",type,".init.csv"));
    iPRS<-readParamsCSV(iCSV);
    ##get final parameter values
    fCSV<-file.path(inp.dir,paste0("TCSAM02.params.",type,".final.csv"));
    fPRS<-readParamsCSV(fCSV);
    
    ##combine initial and final values
    prsObj<-cbind(fPRS,init=iPRS$value);
    prsObj<-prsObj[,c(1:6,10,7:9)];
    class(prsObj)<-c('tcsam02.prs',class(prsObj));#set class attribute to 'tcsam02.prs' for identification
    
    return(prsObj);
}
