#' Function frament of 'TpAUC.function' which calculates bounds in FPR inside a range
fpr.lower.bounds <- function(xsample, ysample, lower.fp, upper.fp){
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
    pAUC.roc <- sum(diff(fpr.pr)*apply(cbind(sen.pr[-1], sen.pr[-length(sen.pr)]), 1, mean))

    diagonal.pAUC <- sum(diff(fpr.pr^2))/2
    TpAUC.min.roc <- sum(diff(fpr.pr))*min(sen.pr)
    TpAUC.min.proper <- max(TpAUC.min.roc, diagonal.pAUC)
    TpAUC.min.dplr <- sum(diff(fpr.pr))*mean(c(min(sen.pr), max(sen.pr)))

    list(
        "diagonal.pAUC" = diagonal.pAUC,
        "TpAUC.min.roc" = TpAUC.min.roc,
        "TpAUC.min.proper" = TpAUC.min.proper,
        "TpAUC.min.dplr" = TpAUC.min.dplr
    )
}

#' Function fragment of 'TpAUC.function' which calculates bounds in FPR inside a range
fpr.bounds <- function(xsample, ysample, lower.fp, upper.fp) {
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
    pAUC.roc <- sum(diff(fpr.pr)*apply(cbind(sen.pr[-1], sen.pr[-length(sen.pr)]),
                                       1, mean))
    diagonal.pAUC <- sum(diff(fpr.pr^2))/2
    TpAUC.min.roc <- sum(diff(fpr.pr))*min(sen.pr)
    TpAUC.min.proper <- max(TpAUC.min.roc, diagonal.pAUC)
    TpAUC.min.dplr <- sum(diff(fpr.pr))*mean(c(min(sen.pr), max(sen.pr)))
    plr.pr <- (sen.pr-sen.pr[1])/(fpr.pr-fpr.pr[1])
    plr.pr <- plr.pr[is.finite(plr.pr)]
    if (all(plr.pr>=plr.pr[length(plr.pr)])) {
        TpAUC.min <- TpAUC.min.dplr
    } else {
        if (all(sen.pr>=fpr.pr)) {
            TpAUC.min <- TpAUC.min.proper
        } else {
            TpAUC.min <- TpAUC.min.roc
        }
    }
    if (min(sen.pr)==max(sen.pr)) {
        warning("Constant ROC curve over the prefixed FPR range")
        TpAUC.max <- sum(diff(fpr.pr))
    } else {
        TpAUC.max <- sum(diff(fpr.pr))*max(sen.pr)
    }
    list(
        tp_auc_max = TpAUC.max,
        tp_auc_min = TpAUC.min
    )
}
