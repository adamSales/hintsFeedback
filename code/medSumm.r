print.summary.mediate <- function(x,...){

    clp <- 100 * x$conf.level
    cat("\n")
    cat(sprintf("Causal Mediation Analysis %s\n\n", ifelse(inherits(x,
        "mediate.tsls"), "using Two-Stage Least Squares", "")))
    if (x$boot) {
        cat(sprintf("Nonparametric Bootstrap Confidence Intervals with the %s Method\n\n",
            ifelse(x$boot.ci.type == "perc", "Percentile", "BCa")))
    }
    else {
        cat(sprintf("%s Confidence Intervals\n\n", ifelse(inherits(x,
            "mediate.tsls"), "Two-Stage Least Squares", "Quasi-Bayesian")))
    }
    if (!is.null(x$covariates)) {
        cat("(Inference Conditional on the Covariate Values Specified in `covariates')\n\n")
    }
    isLinear.y <- ((class(x$model.y)[1] %in% c("lm", "rq")) ||
        (inherits(x$model.y, "glm") && x$model.y$family$family ==
            "gaussian" && x$model.y$family$link == "identity") ||
        (inherits(x$model.y, "survreg") && x$model.y$dist ==
            "gaussian"))
    printone <- !x$INT && isLinear.y
    if (printone) {
        smat <- c(x$d1, x$d1.ci, x$d1.p)
        smat <- rbind(smat, c(x$z0, x$z0.ci, x$z0.p))
        smat <- rbind(smat, c(x$tau.coef, x$tau.ci, x$tau.p))
        smat <- rbind(smat, c(x$n0, x$n0.ci, x$n0.p))
        rownames(smat) <- c("ACME", "ADE", "Total Effect", "Prop. Mediated")
    }
    else {
        smat <- c(x$d0, x$d0.ci, x$d0.p)
        smat <- rbind(smat, c(x$d1, x$d1.ci, x$d1.p))
        smat <- rbind(smat, c(x$z0, x$z0.ci, x$z0.p))
        smat <- rbind(smat, c(x$z1, x$z1.ci, x$z1.p))
        smat <- rbind(smat, c(x$tau.coef, x$tau.ci, x$tau.p))
        smat <- rbind(smat, c(x$n0, x$n0.ci, x$n0.p))
        smat <- rbind(smat, c(x$n1, x$n1.ci, x$n1.p))
        smat <- rbind(smat, c(x$d.avg, x$d.avg.ci, x$d.avg.p))
        smat <- rbind(smat, c(x$z.avg, x$z.avg.ci, x$z.avg.p))
        smat <- rbind(smat, c(x$n.avg, x$n.avg.ci, x$n.avg.p))
        rownames(smat) <- c("ACME (control)", "ACME (treated)",
            "ADE (control)", "ADE (treated)", "Total Effect",
            "Prop. Mediated (control)", "Prop. Mediated (treated)",
            "ACME (average)", "ADE (average)", "Prop. Mediated (average)")
    }
    colnames(smat) <- c("Estimate", paste(clp, "% CI Lower",
        sep = ""), paste(clp, "% CI Upper", sep = ""), "p-value")
    printCoefmat(smat, digits = 3)
    cat("\n")
    cat("Sample Size Used:", x$nobs, "\n\n")
    cat("\n")
    cat("Simulations:", x$sims, "\n\n")
    invisible(smat)
}
