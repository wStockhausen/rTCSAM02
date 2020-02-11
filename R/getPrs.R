#'
#'@title Get a TCSAM02 prs object by reading initial and final parameters csv files
#'
#'@description Function to get a TCSAM02 prs object by reading initial and final parameters csv file.
#'
#'@param type - 'all' or 'active'
#'@param inp.dir - folder for files
#'@param verbose - flag to print information
#'
#'@return A prs model object (a dataframe). The returned object will be a list of class 'tcsam02.prs'.
#'
#'@details To create the prs object, this function reads 2 csv-type parameter value files,
#'one associated with the initial parameter values and one associated with the final values
#'(tcsam02...init.csv and tcsam02...final.csv, respectively).
#'The user can select to return either "all" the parameters, or only the "active" parameters (ones
#'with phase > 0). The returned object will be a dataframe of class 'tcsam02.prs'.
#'
#'@export
#'
getPrs<-function(type='all',inp.dir='.',verbose=FALSE){
    if (verbose) cat("rTCSAM02::getPrs(): Starting...\n")
    options(stringsAsFactors=FALSE);
    if (!any(type==c('all','active'))) {
        cat("type = '",type,"' undefined for function rTCSAM02::getPrs(...).\n",sep='');
        cat("Returning NULL.\n\n");
        return(NULL);
    }
    if (is.null(inp.dir)) {
      inp.dir<-tcltk::tk_choose.dir(caption="Select folder with parameter CSV files");
    }
    ##get initial parameter values
    iCSV<-file.path(inp.dir,paste0("tcsam02.params.",type,".init.csv"));
    if (file.exists(iCSV)){
        if (verbose) cat("rTCSAM02::getPrs(): Reading \n\t'",iCSV,"'.\n",sep='')
        iPRS<-readParamsCSV(iCSV);
    } else {
        cat("rTCSAM02::getPrs(): \n\t'",iCSV,"'\ndoes not exist.\n",sep='')
        cat("Returning NULL\n");
        return(NULL);
    }
    ##get final parameter values
    fCSV<-file.path(inp.dir,paste0("tcsam02.params.",type,".final.csv"));
    if (file.exists(fCSV)){
        if (verbose) cat("rTCSAM02::getPrs(): Reading \n\t'",fCSV,"'.\n",sep='')
        fPRS<-readParamsCSV(fCSV);
        farithvals<-fPRS[["value_arith"]];
        fparamvals<-fPRS[["value_param"]];
    } else {
        cat("rTCSAM02::getPrs(): \n\t'",fCSV,"'\ndoes not exist.\n",sep='');
        farithvals<-NULL;
        fparamvals<-NULL;
    }

    ##combine initial and final values
    prsObj<-cbind(iPRS[,c("category","process","label","type","name",
                          "index","phase","min_index","max_index","parameter_scale",
                          "min_arith","max_arith")],init_arith_value=iPRS[["value_arith"]],final_arith_value=farithvals,
                  iPRS[,c("min_param","max_param")],init_param_value=iPRS[["value_param"]],final_param_value=fparamvals);
    class(prsObj)<-c('tcsam02.prs',class(prsObj));#set class attribute to 'tcsam02.prs' for identification

    if (verbose) cat("rTCSAM02::getPrs(): Done!\n")
    return(prsObj);
}
