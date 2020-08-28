#'
#'@title Function to plot ABC and OFL calculations from mcmc.CalcABC(...)
#'
#'@description This function plots ABC and OFL from TCSAM02 mcmc output.
#'
#'@param dfrABC - dataframe with selected values for ABC.buff, ABC.pstar, and OFL by case
#'@param dfrOFL - empirical cdfs for OFL, by case
#'@param xlims - range for x-axis in plot
#'
#'@return ggplot object
#'
#'@import ggplot2
#'
#'@details Inputs dfrABCs and dfrOFLs are outputs from \code{mcmc.CalcABC}
#'
#'@export
#'
mcmc.PlotABC<-function(dfrABCs,dfrOFLs,xlims=NULL){
    #--plot results
    if (is.null(xlims)){
        xmin<-min((dfrABCs$ABC.buff-2),dfrOFLs$OFLs);
        xlims<-c(xmin,NA);
    }
    p <- ggplot(data=dfrOFLs,mapping=aes_string(x="OFLs",y="probs",colour="case"));
    p <- p + geom_line();
    p <- p + geom_vline(data=dfrABCs,mapping=aes_string(xintercept="OFL",colour="case"),size=2,alpha=0.5,show.legend=FALSE);
    p <- p + geom_vline(data=dfrABCs,mapping=aes_string(xintercept="ABC.pstar",colour="case"),size=1,linetype=2,show.legend=FALSE);
    p <- p + geom_vline(data=dfrABCs,mapping=aes_string(xintercept="ABC.buff",colour="case"),size=1,linetype=2,show.legend=FALSE);
    p <- p + geom_text(data=dfrABCs,mapping=aes_string(x="OFL",y=0.9,colour="case"),label="OFL",alpha=0.5,show.legend=FALSE,hjust="right");
    p <- p + geom_text(data=dfrABCs,mapping=aes_string(x="ABC.pstar",y=0.8,colour="case"),label="p-star ABC",show.legend=FALSE,hjust="right");
    p <- p + geom_text(data=dfrABCs,mapping=aes_string(x="ABC.buff",y=0.8,colour="case"),label="buffer ABC",show.legend=FALSE,hjust="right");
    p <- p + geom_hline(yintercept=1,linetype=3);
    p <- p + labs(x="1000's t",y="empirical cdf");
    p <- p + xlim(xlims);
    return(p)
}
