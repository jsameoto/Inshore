# Here's a quick version of the SSModle pred.eval function. DK should add comments and make it pretty...
#JS comments - had to create this since in 2020 didn't have survey but to do prediction evaluations had to run a model for 2020 and then made biomass posterior NAs -- but eval.predict() in SSModel does not remove NAs from calculation in quantile function call. 
#error from trying this with eval.predict is  < Error in quantile.default(newX[, i], ...) : missing values and NaN's not allowed if 'na.rm' is FALSE > 
#Needed to edit eval.predict function to have na.rm = TRUE in quantile call or in the apply call that calls the quantile function - rather than update function within package SSModel, we are now using this modified function outside SSModel 

eval.predict <- function (x, plot = TRUE, Year = 2000, Box.col = "blue", 
                          pred.lim = NULL) 
{
  no.years = length(x)
  temp <- x[[1]]$B.next
  for (i in 2:no.years) {
    temp <- cbind(temp, x[[i]]$B.cur, x[[i]]$B.next)
  }
  no.box <- dim(temp)[2]
  out1 <- boxplot(temp[, seq(1, no.box, by = 2)], range = 0, 
                  plot = F)
  out2 <- boxplot(temp[, seq(2, no.box, by = 2)], range = 0, 
                  plot = F)
  if (plot) {
    if (is.null(pred.lim)) {
      bxp(out1, xaxt = "n", ylab = "Commercial biomass (t, meats)", 
          boxwex = 0.2)
      bxp(out2, add = TRUE, boxwex = 0.2, at = 0.25 + 1:floor(no.box/2), 
          xaxt = "n", boxfill = Box.col)
      axis(side = 1, labels = Year + (0:floor(no.box/2)), 
           at = (1:ceiling(no.box/2)) + 0.12)
      mtext("Year", 1, 2.5)
    }
    else {
      out1$stats[c(1, 5), ] <- apply(temp[, seq(1, no.box, 
                                                by = 2)], 2, FUN = quantile, probs = c(pred.lim/2, 1 - pred.lim/2),na.rm=T)
      out2$stats[c(1, 5), ] <- apply(temp[, seq(2, no.box, 
                                                by = 2)], 2, FUN = quantile, probs = c(pred.lim/2, 1 - pred.lim/2),na.rm=T)
      bxp(out1, xaxt = "n", ylab = "Commercial biomass (t, meats)", 
          boxwex = 0.2)
      bxp(out2, add = TRUE, boxwex = 0.2, at = 0.25 + 1:floor(no.box/2), 
          xaxt = "n", boxfill = Box.col)
      axis(side = 1, labels = Year + (0:floor(no.box/2)), 
           at = (1:ceiling(no.box/2)) + 0.12)
      mtext("Year", 1, 2.5)
    }
  }
  else {
    if (!is.null(pred.lim)) {
      out1$stats[c(1, 5), ] <- apply(temp[, seq(1, no.box, 
                                                by = 2)], 2, quantile, probs = c(pred.lim/2, 
                                                                                 1 - pred.lim/2))
      out2$stats[c(1, 5), ] <- apply(temp[, seq(2, no.box, 
                                                by = 2)], 2, quantile, probs = c(pred.lim/2, 
                                                                                 1 - pred.lim/2))
    }
    out1$names <- out2$names <- ""
    return(list(Next = out1[1:3], Cur = out2[1:3]))
  }
}