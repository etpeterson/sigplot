#ETP 20170412
#this function accepts a list of fits and generates a visual matrix of significance
#library(corrplot)
#library(ggcorrplot)
#library(plyr)
#library(reshape2)



fnum2str <- function(numdf, c=0.05, fstr="%0.2f", estr="%0.1e"){
  numstr <- ldply(numdf,
                  function(num)
                    if(is.na(num)){sprintf("N/A")}
                    else if(num<=c){sprintf(estr,num)}
                    else{sprintf(fstr,num)})
  colnames(numstr) <- "value_str"
  return(numstr)
}

sigframe <- function(fitlist, coef = "Pr(>|t|)"){  #coef = "Estimate" gives the direction of significance
  corrlist <- lapply(fitlist,
                     function(ft)
                       data.frame(t(data.frame(summary(ft)$coefficients[,coef]))))
  fitdf <- rbind.fill(corrlist)
  rownames(fitdf) <- names(corrlist)
  return(fitdf)
}

sigplot <- function(fitdf, pthresh=0.05, alphasort=TRUE, region_labels=NULL, 
                    variable_labels=NULL, drop_variables=NULL){
  if (class(fitdf) == "list"){
    hascoef <- TRUE
    fitcoef <- sigframe(fitdf, "Estimate")
    fitdf <- sigframe(fitdf)
    fitmatcoef <- as.matrix(fitcoef)
    fitmeltcoef <- melt(fitmatcoef, varnames = c("Region","Variable"))
    if (!is.null(drop_variables)){
      fitmeltcoef <- fitmeltcoef[-grep(paste(drop_variables,collapse="|"),
                                       fitmeltcoef$Variable,ignore.case=TRUE),]
    }
  }else{
    hascoef <- FALSE
  }
  fitmat <- as.matrix(fitdf)
  fitmelt <- melt(fitmat, varnames = c("Region","Variable"))
  if (!is.null(drop_variables)){
    fitmelt <- fitmelt[-grep(paste(drop_variables,collapse="|"),
                             fitmelt$Variable,ignore.case=TRUE),]
  }
  if (alphasort){
    if (is.null(region_labels)){
      fitmelt$Region <- factor(fitmelt$Region, 
                               levels=sort(levels(fitmelt$Region), 
                                           decreasing=TRUE))
    }else{
      fitmelt$Region <- factor(fitmelt$Region, 
                               levels=names(sort(region_labels, 
                                                 decreasing=TRUE)))
    }
    if (is.null(variable_labels)){
      fitmelt$Variable <- factor(fitmelt$Variable, 
                                 levels=sort(levels(fitmelt$Variable)))
    }else{
      fitmelt$Variable <- factor(fitmelt$Variable, 
                                 levels=names(sort(variable_labels)))
    }
  }else{
    fitmelt$Region <- factor(fitmelt$Region)
    fitmelt$Variable <- factor(fitmelt$Variable)
  }
  fitmelt <- cbind(fitmelt, fnum2str(fitmelt$value, c=pthresh))
  if (hascoef){
    est <- sign(fitmeltcoef$value)
    est[is.na(est)] <- 0
    fitmelt <- cbind(fitmelt, est=est)
  }
  lims <- c(0, 0.2)
  breakvec <- c(0, 0.01, 0.05, 0.075, 0.1, 0.15, 0.2)
  #breakvec <- c(0, 0.1, 0.2)
  breakvecnorm <- breakvec/max(lims)
  cvec <- c("white", "cyan", "blue", "black")
  #cvec <- c("black", "white")
  gcp <- ggplot(fitmelt, aes(x=Variable, y=Region, fill=value)) + 
    geom_tile(color = "red") +
    scale_fill_gradientn(colors = cvec, breaks = breakvec, 
                         values = breakvecnorm, limits = lims, name = "p-value",
                         na.value = "black") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
          legend.key.height = unit(0.1, "npc"), 
          legend.background = element_rect(fill = "gray95")) +
    coord_fixed()
  if (!is.null(region_labels)){
    gcp <- gcp + scale_y_discrete(labels=region_labels)
  }
  if (!is.null(variable_labels)){
    gcp <- gcp + scale_x_discrete(labels=variable_labels)
  }
  if (hascoef){
    positions <- data.frame(id=rep(seq(nrow(fitmelt)), each=3), 
                            x_tri=c(rbind(as.numeric(fitmelt$Variable)-0.15,
                                          as.numeric(fitmelt$Variable),
                                          as.numeric(fitmelt$Variable)+0.15)), 
                            y_tri=c(rbind(
                              as.numeric(fitmelt$Region)+fitmelt$est*0.2,
                              as.numeric(fitmelt$Region)+fitmelt$est*0.45,
                              as.numeric(fitmelt$Region)+fitmelt$est*0.2)), 
                            value_black=rep(1, nrow(fitmelt)))
    gcp <- gcp + geom_polygon(data=positions, aes(x=x_tri, y=y_tri, group=id, 
                                                  fill=value_black))
  }
  gcp <- gcp + geom_text(aes(x=Variable, y=Region, label = value_str), 
                         color = "red", size = 3) #color = "red"
  return(gcp)
}