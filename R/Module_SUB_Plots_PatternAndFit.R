#' plotPattern
#'
#' Function to plot time series pattern with minimal labels
#' @param yrs numeric vector of years  
#' @param vals numeric vector of values
#' @param width line width, default is 1
#' @param color line color, default is "grey"
#' @param yrs.axis if TRUE, display the year axis. default is FALSE 
#' @param vals.axis if true, display the value axis. default is FALSE 
#' @param vals.lim vector of length 2 with y-axis limits
#' @param hgrid if TRUE, display horizontal grid lines. default is FALSE 
#' @param vgrid if TRUE, display vertical grid lines. default is FALSE 
#' @param pch.val numeric value for plotting character. default is 19 (solid pt)
#' @param pch.bg  text string with color for point fill (if pch allows, e.g. 21)
#' @export


plotPattern <- function(yrs,vals,width=1,color="grey",yrs.axis=FALSE,vals.axis=FALSE,
			vals.lim=NULL, hgrid=FALSE,vgrid=FALSE,pch.val=19,pch.bg=NULL){
options(scipen=3)

##############################################################################
# 

# TO DO: 
# - add option to label hgrid with modified values (e.g. plot logs, but label with raw numbers)


if(is.null(vals.lim)){vals.lim <- c(0,max(pretty(vals)))}

plot(yrs,vals,axes=FALSE,ylim=vals.lim,type="l",lwd=width,col=color,xlab="",ylab="")

if(yrs.axis==TRUE){axis(side=1,at=pretty(yrs),labels=pretty(yrs),cex.axis=1,lwd=0,lwd.tick=1)}

if(vals.axis==TRUE){

	axis(side=2,at=pretty(vals.lim),labels=NA,cex.axis=1,lwd=0,lwd.tick=1,xpd=NA)
	y.labels<-pretty(vals.lim)
	yrs.range <- max(yrs)-min(yrs)
	text(rep(min(yrs)-(yrs.range*0.065),length(y.labels)),y.labels,labels=prettyNum(y.labels,big.mark=","),xpd=NA,cex=1,adj=1)
	}

if(hgrid==TRUE){
	ticks<-pretty(vals.lim)
	segments(rep(min(yrs),length(ticks)),ticks,rep(max(yrs),length(ticks)),ticks ,lty=2,col="lightgrey",xpd=NA)
	}

if(vgrid[1]!=FALSE){abline(v=vgrid,col="red",lty=2)	  }

lines(yrs, vals,lwd=width,col=color) # replot line to "move it in front of hgrid and v grid

# add a simple point with whichever symbol specified by pch.val
if(is.null(pch.bg)){points(yrs, vals,col=color,pch=pch.val,cex=1)} # NEW LAYOUT: larger points
# add a two-cloured point iv pch.bg is specified -> valid only for pch = 21-25
if(!is.null(pch.bg)){points(yrs, vals,col=color,pch=pch.val,cex=1,bg=pch.bg)}

}



#' addFit
#'
#' Function to plot time series pattern with minimal labels
#' @param data.df data frame used for the slope calculation that produced the coefficients (using the Year column here)
#' @param coeff are the coefficients (median from posterior or MLE values)
#' @param col line color, default is "red"
#' @param lwd line width, default is 2
#' @export


addFit <- function(data.df, coeff,col = "red",lwd = 2){
# 
# To Do: add bounds for MCMC versions

segments(min(data.df$Year), coeff$intercept ,
         max(data.df$Year), coeff$intercept + length(data.df$Year) * coeff$slope , pch=19,col=col,lwd=lwd)


}




