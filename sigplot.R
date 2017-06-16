#ETP 20170412
#this function accepts a list of fits and generates a visual matrix of significance
library(corrplot)
library(ggcorrplot)
library(plyr)
library(reshape2)

fnum2str <- function(numdf, c=0.05, fstr="%0.2f", estr="%0.1e"){
  numstr <- ldply(numdf,function(num)if(is.na(num)){sprintf("N/A")}else if(num<=c){sprintf(estr,num)}else{sprintf(fstr,num)})
  colnames(numstr) <- "value_str"
  return(numstr)
}

sigframe <- function(fitlist, coef = "Pr(>|t|)"){  #coef = "Estimate" gives the direction of significance
  corrlist <- lapply(fitlist,function(ft)data.frame(t(data.frame(summary(ft)$coefficients[,coef]))))
  fitdf <- rbind.fill(corrlist)
  rownames(fitdf) <- names(corrlist)
  return(fitdf)
}

sigplot <- function(fitdf){
  if (class(fitdf) == "list"){
    hascoef <- TRUE
    fitcoef <- sigframe(fitdf, "Estimate")
    fitdf <- sigframe(fitdf)
    fitmatcoef <- as.matrix(fitcoef)
    fitmeltcoef <- melt(fitmatcoef, varnames = c("Region","Variable"))
  }else{
    hascoef <- FALSE
  }
  fitmat <- as.matrix(fitdf)
  fitmelt <- melt(fitmat, varnames = c("Region","Variable"))
  fitmelt <- cbind(fitmelt, fnum2str(fitmelt$value))
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
  gcp <- ggplot(fitmelt, aes(x=Variable, y=Region, fill=value)) + geom_tile(color = "red") +
    scale_fill_gradientn(colors = cvec, breaks = breakvec, values = breakvecnorm, limits = lims, name = "p-value", na.value = "black") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), legend.key.height = unit(0.1, "npc"), legend.background = element_rect(fill = "gray95")) +
    coord_fixed()
  if (hascoef){
    #gcp <- gcp + geom_segment(aes(x=as.numeric(Variable),xend=as.numeric(Variable),y=as.numeric(Region)+est*0.25,yend=as.numeric(Region)+est*0.45),arrow=arrow(type="closed",length=unit(0.1,"native"))) #0.03 or 0.1, why changing?
    positions <- data.frame(id=rep(seq(nrow(fitmelt)), each=3), x_tri=c(rbind(as.numeric(fitmelt$Variable)-0.15,as.numeric(fitmelt$Variable),as.numeric(fitmelt$Variable)+0.15)), y_tri=c(rbind(as.numeric(fitmelt$Region)+fitmelt$est*0.2,yend=as.numeric(fitmelt$Region)+fitmelt$est*0.45,as.numeric(fitmelt$Region)+fitmelt$est*0.2)), value_black=rep(1, nrow(fitmelt)))
    gcp <- gcp + geom_polygon(data=positions, aes(x=x_tri, y=y_tri, group=id, fill=value_black))
  }
  gcp <- gcp + geom_text(aes(x=Variable, y=Region, label = value_str), color = "red", size = 3) #color = "red"
  #gcp <- gcp + geom_text(aes(x=Variable, y=Region, label = sprintf("%0.2f", round(value, digits = 2))), color = "red", size = 3) #color = "red"
  return(gcp)
}