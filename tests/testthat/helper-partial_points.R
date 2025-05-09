#' Function fragment of 'TpAUC.function' which calculates indexes of ratios inside a range
partial.points.indexes <- function(xsample, ysample, lower.fp, upper.fp){
    if (lower.fp>=upper.fp) {
        stop("Error in the prefixed FPR range")
    }

    fpr.roc <- points.curve(xsample, ysample)[,1]
    sen.roc <- points.curve(xsample, ysample)[,2]

    if (is.unsorted(sen.roc)) {
        sen.roc <- rev(sen.roc)
        fpr.roc <- rev(fpr.roc)
    }
    i.l <- min(which(fpr.roc>=lower.fp))
    i.u <- max(which(fpr.roc<=upper.fp))
    c(
        "lower" = i.l,
        "upper" = i.u
    )
}

#' Function fragment of 'TpAUC.function' which calculates partial points in range
partial.points.curve <- function(xsample, ysample, lower.fp, upper.fp) {
    if (lower.fp>=upper.fp) {
        stop("Error in the prefixed FPR range")
    }

    fpr.roc <- points.curve(xsample, ysample)[,1]
    sen.roc <- points.curve(xsample, ysample)[,2]

    if (is.unsorted(sen.roc)) {
        sen.roc <- rev(sen.roc)
        fpr.roc <- rev(fpr.roc)
    }
    i.l <- min(which(fpr.roc>=lower.fp))
    j.l <- max(i.l-1,1)
    i.u <- max(which(fpr.roc<=upper.fp))
    j.u <- min(1+i.u, length(fpr.roc))

    fpr.pr <- fpr.roc[i.l:i.u]
    sen.pr <- sen.roc[i.l:i.u]

    if (fpr.roc[i.l]>lower.fp) {
        fpr.pr <- append(fpr.pr, lower.fp, 0)
        lscale <- (fpr.pr[1]-fpr.roc[j.l])/(fpr.roc[i.l]-fpr.roc[j.l])
        sen.pr <- append(sen.pr,sen.roc[j.l]+(sen.roc[i.l]-sen.roc[j.l])*lscale, 0)
    }
    if (fpr.roc[i.u]<upper.fp) {
        fpr.pr <- append(fpr.pr, upper.fp, length(fpr.pr))
        uscale <- (fpr.roc[j.u]-fpr.pr[length(fpr.pr)])/(fpr.roc[j.u]-fpr.roc[i.u])
        sen.pr <- append(sen.pr, sen.roc[j.u]-(sen.roc[j.u]-sen.roc[i.u])*uscale,
                         length(sen.pr))
    }
    tibble::tibble(sen.pr, fpr.pr)
}

#' Function fragment of 'FpaucHS' which calculates partial points in HS range
pHSpoints <- function(xsample, ysample, lower.sen) {
    pts.roc=NULL; fpr.roc=NULL; sen.roc=NULL; fpr.p=NULL; sen.p=NULL;
    ppoints=NULL;
    i.low=NULL; j.low=NULL; lscale=NULL;
    if ((lower.sen >= 1) || (lower.sen<0)) {stop("Error in the prefixed TPR
 range")}
    pts.roc <- ROCpoints(xsample, ysample)
    fpr.roc <- pts.roc[,1]
    sen.roc <- pts.roc[,2]
    i.low <- min(which(sen.roc >= lower.sen))
    j.low <- max(i.low -1, 1)
    fpr.p <- fpr.roc[i.low:length(fpr.roc)]
    sen.p <- sen.roc[i.low:length(sen.roc)]
    if ((sen.roc[i.low] > lower.sen) && (i.low>1)) {
        sen.p <- append(sen.p, lower.sen, 0)
        lscale <- (sen.p[1]-sen.roc[j.low])/(sen.roc[i.low]-sen.roc[j.low])
        fpr.p <- append(fpr.p, fpr.roc[j.low]+(fpr.roc[i.low]-
                                                   fpr.roc[j.low])*lscale, 0)}
    ppoints=cbind(fprp=fpr.p, tprp=sen.p)
    return(ppoints)}
