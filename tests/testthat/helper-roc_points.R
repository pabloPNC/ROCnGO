# Preliminary function to use in the TpAUC function
# points.curve function calculates the coordinates (FPR, TPR) of the ROC curve 
# Inputs:
#   xsample : binary variable with 1 indicating success occurs
#   ysample : variable of the diagnostic test results
# Output:
#   xy.roc : pairs of points (fpr, tpr) of the ROC curve
#
points.curve <- function(xsample, ysample) {
    fpr.p <- NULL; sen.p <- NULL
    x.p <- xsample[which(is.na(xsample)==FALSE & is.na(ysample)==FALSE)]
    y.p <- ysample[which(is.na(xsample)==FALSE & is.na(ysample)==FALSE)]


    pts <- sort(y.p)
    pts <- append(pts[-length(pts)]+diff(pts)/2, min(pts)-1, 0)
    pts <- append(pts, max(y.p)+1, length(pts))

    for (i.pt in 1:length(pts)) {
        pt <- pts[i.pt]
        pre.p <- (y.p>pt)*1
        fpr.p[i.pt] <- sum((pre.p==1)*(x.p==0))/sum(x.p==0)
        sen.p[i.pt] <- sum((pre.p==1)*(x.p==1))/sum(x.p==1)
    }

    if (is.unsorted(sen.p)) {
        sen.p <- rev(sen.p)
        fpr.p <- rev(fpr.p)
    }
    xy.roc <- cbind(fpr.p,sen.p)
    return(xy.roc)
}

#' Defined function to check points
points.thresholds <- function(xsample, ysample) {
    x.p <- xsample[which(is.na(xsample) == FALSE & is.na(ysample) == FALSE)]
    y.p <- ysample[which(is.na(xsample) == FALSE & is.na(ysample) == FALSE)]

    pts <- sort(y.p)
    pts <- append(pts[-length(pts)] + diff(pts) / 2, min(pts) - 1, 0)
    pts <- append(pts, max(y.p) + 1, length(pts))
}