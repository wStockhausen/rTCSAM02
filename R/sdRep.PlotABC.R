#'
#'@title Plot ABC and OFL from [rTCSAM02::sdRep.CalcABC()] output
#'
#'@description Function to plot ABC and OFL from [rTCSAM02::sdRep.CalcABC()] output.
#'
#'@param dfrABC - dataframe with selected values for ABC.buff, ABC.pstar, and OFL by case
#'@param xlims - range for x-axis in plot
#'
#'@return ggplot object
#'
#'@import magrittr
#'@import dplyr
#'@import ggplot2
#'@import cowplot
#'@import wtsPlots
#'
#'@details Input dfrABCs should be an output dataframe from [rTCSAM02::sdRep.CalcABC()]
#'
#'@export
#'@md
#'
sdRep.PlotABC<-function(dfrABCs,xlims=NULL){
  cases = unique(dfrABCs$case);

  if (is.null(xlims)){
      xmin<-min(dfrABCs$ABC.buff-2);
      xlims<-c(xmin,NA);
  }

  lst = list();
  for (case_ in cases) {
    #--testing: case_=cases[1]
    rwABC = dfrABCs %>% dplyr::filter(case==case_);
    estOFL = rwABC$OFL;
    stdOFL = rwABC$stdvOFL;
    ps    = seq(from=0.01,to=0.99,by=0.01);
    qs    = qnorm(ps,mean=estOFL,sd=stdOFL,lower.tail=TRUE);
    lst[[case_]] = rwABC %>% dplyr::full_join(tibble::tibble(p=ps,q=qnorm(ps,mean=estOFL,sd=stdOFL,lower.tail=TRUE)),
                                         by=character());
  }
  dfrDists = dplyr::bind_rows(lst);

  #--make plot
  p1 = ggplot(mapping=aes(colour=case)) +
          geom_line(data=dfrDists,mapping=aes(x=q,y=p));#--dummy plot to get legend
  lg = cowplot::get_legend(p1); #--get legend
  p2 = ggplot(mapping=aes(colour=case)) +
          geom_line(data=dfrDists,mapping=aes(x=q,y=p)) +
          geom_vline(data=dfrABCs,mapping=aes(xintercept=OFL,colour=case)) +
          geom_vline(data=dfrABCs,mapping=aes(xintercept=ABC.pstar),colour="black") +
          geom_vline(data=dfrABCs,mapping=aes(xintercept=ABC.buff,colour=case),linetype=2,size=0.5) +
          geom_text(data=dfrABCs,mapping=aes(x=OFL),y=0.9,label="OFL",fontface="bold",hjust="left",nudge_x=0.1) +
          geom_text(data=dfrABCs,mapping=aes(x=ABC.pstar),y=0.9,label="p^star~~ABC",fontface="bold",hjust="right",colour="black",parse=TRUE) +
          geom_text(data=dfrABCs,mapping=aes(x=ABC.buff), y=0.8,label="buffer\nABC",fontface="bold",hjust="left",nudge_x=0.1,lineheight=0.7) +
          facet_grid(case~.) + xlim(xlims) +
          labs(y="",x="Catch Biomass (1000's t)") +
          wtsPlots::getStdTheme() +
          theme(legend.position="none",
                axis.text.y=element_blank());
  pg = cowplot::plot_grid(p2,lg,nrow=1,rel_widths=c(6,1));#--combine plot with legend
  return(pg);
}
