function (include.answer, seed) {

    set.seed(seed)

    ## Create sample
    n <- sample(4:6, 1)
    df <- n - 1
    X <- makesample(n)
    Y <- makesample(n)

    ## Get rcrit
    alpha <- sample(c(0.2, 0.1), 1)

    paste0("Calculate $r_{\\mathit{XY}}$ and test H_0: \\rho_{\\mathit{XY}} = 0 at \\alpha = ", alpha, ".") %>>% cat(sep="\n")

    if (include.answer == TRUE) {

        Xsd <- round(sd(X), 2)
        Ysd <- round(sd(Y), 2)
        Xbar <- round(mean(X), 2)
        Ybar <- round(mean(Y), 2)

        cbind(X, Y) %>>%
            as.data.frame() %>>%
            mutate(Xdev = X - Xbar) %>>%
            mutate(Ydev = Y - Ybar) %>>%
            mutate(Xdev.sq = Xdev^2) %>>%
            mutate(Ydev.sq = Ydev^2) %>>%
            mutate(product = Xdev * Ydev) %>>%
            (~ tmp) %>>%
            rename("$X_i$"=X,
                   "$Y_i$"=Y,
                   "$X_i - \\bar{X}$"=Xdev,
                   "$Y_i - \\bar{Y}$"=Ydev,
                   "$(X_i - \\bar{X})^2$"=Xdev.sq,
                   "$(Y_i - \\bar{Y})^2$"=Ydev.sq,
                   "$(X_i - \\bar{X})(Y_i - \\bar{Y})$"=product) %>>%
            xtable(digits=0) %>>%
            print.xtable(floating=TRUE,
                         table.placement="!h",
                         sanitize.text.function=function(x){x},
                         booktabs=TRUE,
                         include.rownames=FALSE)

        SSx <- sum(tmp$Xdev.sq)
        SSy <- sum(tmp$Ydev.sq)
        SP <- sum(tmp$product)
        r <- round(SP / sqrt(SSx * SSy), 2)
        p <- round(cor.test(X,Y)$p.value, 2)
        rcrit <- round(rcrit(n, alpha), 2)

        paste0("
            \\vspace{-3em}
            \\begin{multicols}{2}
            \\begin{gather*}
            \\bar{X} = ", Xbar, " \\\\
            \\bar{Y} = ", Ybar, " \\\\
            \\mathit{SS_X} = ", SSx, " \\\\
            \\end{gather*}
            \\begin{gather*}
            \\\\
            \\mathit{SS_Y} = ", SSy, " \\\\
            \\mathit{SP} = ", SP, " \\\\
            r_{\\mathit{XY}} = ", SP, " / \\sqrt{", SSx, " \\times ", SSy, "} = ", r, " \\\\
            r_{\\textnormal{crit}} = ", rcrit, "
            \\end{gather*}
            \\end{multicols}") %>>% cat(sep="\n")

    } else {

        cbind(X, Y) %>>%
            as.data.frame() %>>%
            rename("$X_i$"=X,
                   "$Y_i$"=Y) %>>%
            xtable() %>>%
            print.xtable(floating=TRUE,
                         table.placement="!h",
                         sanitize.text.function=function(x){x},
                         booktabs=TRUE,
                         include.rownames=FALSE)
    }
}
