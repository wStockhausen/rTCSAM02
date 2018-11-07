#'
#' @title Read the model cor (correlation) file
#'
#' @description Function to read a model cor (correlation) file.
#'
#' @param folder - folder containing the model run
#' @param model - model name (default is "tcsam02")
#'
#' @return a dataframe
#'
#' @details This function reads the model par and cor files, based on the given folder and model name, and
#' returns a dataframe with columns i, p_i, type, p_j, and val. "p_i" and "p_j" are the parameter names, and
#' "type" reflects the value ("est", "sdv", or "cor").
#'
#' @export
#'
readCorFile<-function(folder=NULL,
                      model="tcsam02"){
    options(stringsAsFactors=FALSE);
    if (is.null(folder)){
        folder<-tcltk::tk_choose.dir(default=getwd(),
                                     caption="Select TCSAM02 model results folder");
    }
    #get the number of active parameters from the par file
    par<- readLines(con=file.path(folder,paste0(model,".par")),n=1);
    np <- as.numeric(stringr::str_extract(par,"(\\d+)"));#number of parameters

    #read the cor file
    corF<- readLines(con=file.path(folder,paste0(model,".cor")),n=2+np);
    corP <- stringr::str_split(corF[3:(2+np)],stringr::regex("[:blank:]"));
    params<-vector(length=np,mode="character");
    nr <- 2*np + 0.5*np*(np+1);
    dfr<-data.frame(i   =vector(length=nr,mode="integer"),
                    p_i =vector(length=nr,mode="character"),
                    type=vector(length=nr,mode="character"),
                    j   =vector(length=nr,mode="integer"),
                    p_j =vector(length=nr,mode="character"),
                    val =vector(length=nr,mode="numeric"),
                    stringsAsFactors=FALSE);
    f<-function(strv,k,p,type,j,idx){
      dfr$i[k]    <<- p;
      dfr$p_i[k]  <<- strv[2];
      dfr$type[k] <<- type;
      dfr$j[k]    <<- j;
      dfr$p_j[k]  <<- "";
      dfr$val[k]  <<- as.numeric(strv[idx]);
    }
    k<-0;
    for (p in 1:np){
      strv<-corP[[p]][corP[[p]]!=""];
      params[p]<-strv[2];
      k<-k+1; f(strv,k,p,"est",p,3);
      k<-k+1; f(strv,k,p,"sdv",p,4);
      for (j in 1:p){
        k<-k+1; f(strv,k,p,"cor",j,j+4);
      }#--j
    }#--p

    #add indexes to vector parameter names
    k <- 0; #set counter for vector parameters to 0
    p <- 1; #set parameter index to 1
    while (p<np){
      p.curr<-params[p];
      p.next<-params[p+1];
      if (p.curr!=p.next){
        #params[p] is not a vector , so no change to params[p]
        cat(p,"=",params[p],"\n")
        p<-p+1;
      } else {
        #params[p] is a vector , so need to add index to params[p]
        k<-k+1;
        p.old <- p.curr;
        while (p.next==p.old){
          params[p]<-paste0(params[p],"[",k,"]");
          cat(p,"=",params[p],"\n")
          k <- k+1;#increment counter
          p <- p+1;#increment parameter index
          if (p<=np){
            p.next<-params[p];
          } else {p.next<-"";}
        }
        #params[p] is not the same vector
        k<-0;#reset counter
      }
    }
    if (p==np) {
      #last parameter
      cat(p,"=",params[p],"\n")
    }

    #replace parameter names
    dfr$p_i <- params[dfr$i];
    dfr$p_j <- params[dfr$j];

    return(dfr);
}
