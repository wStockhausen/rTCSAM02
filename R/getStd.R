#'
#'@title Get a TCSAM2015 std object by reading a .std file
#'
#'@description Function to get a TCSAM2015 std object by reading a .std file
#'
#'@param stdFile - std file from a TCSAM2015 model run. can be NULL.
#'@param verbose - flag (T/F) to prnt diagnostic info
#'
#'@return std model object (a list). The returned object will be a list of class 'tcsam2015.std'.
#'
#'@details If \code{stdFile} is NULL, the user will be prompted to identify a
#'TCSAM2015 model report file from which to source the results object.
#'The returned object will be a list of class 'tcsam2015.std'.
#'
#'@export
#'
getStd<-function(stdFile=NULL,verbose=FALSE){
    if (verbose) cat("Starting rTCSAM02::getStd().\n")
    options(stringsAsFactors=FALSE);
    if (is.null(stdFile)){
        stdFile<-wtsUtilities::selectFile(ext='std',caption="Select TCSAM02 std file");
    }
    stdObj<-NULL;
    if (!is.null(stdFile)&&file.exists(stdFile)) {
        stdObj = read.table(stdFile,as.is=T,header=F,skip=1);
        colnames(stdObj)<-c("row id","name","est","std.dev")
        class(stdObj)<-c('tcsam02.std',class(stdObj));#set class attribute to 'tcsam02.std' for identification
    } else {
        cat('rTCSAM02::getSTD(): No std file specified, or specified file does not exist.\n',
            'Returning NULL...\n');
        return(NULL);
    }
    if (verbose) cat("Finished rTCSAM02::getStd().\n")
    return(stdObj);
}
