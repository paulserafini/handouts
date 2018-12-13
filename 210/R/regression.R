function (include, seed) {

    set.seed(seed)

    ## Create sample
    n <- sample(4:6, 1)
    X <- makesample(n)
    Y <- makesample(n)

    ## Calculate regression
    Xsd <- round(sd(X), 2)
    Ysd <- round(sd(Y), 2)
    Xbar <- round(mean(X), 2)
    Ybar <- round(mean(Y), 2)
    Xdev <- X - Xbar
    Ydev <- Y - Ybar
    SSx <- sum(Xdev^2)
    SSy <- sum(Ydev^2)
    product <- round(Xdev * Ydev, 2)
    SP <- round(sum(product), 2)
    B1 <- round(SP / SSx, 2)
    B0 <- round(Ybar - B1 * Xbar, 2)
    Yhat <- B0 + X * B1

    ## Calculate Fcrit
    SStot <- round(sum((Y - Ybar)^2), 2)
    SSreg <- round(sum((Yhat - Ybar)^2), 2)
    SSres <- round(round(SStot - SSreg, 2), 2)
    df1 <- 1
    df2 <- n - df1 - 1
    MSreg <- round(SSreg / df1, 2)
    MSres <- round(SSres / df2, 2)
    alpha <- sample(c(0.05, 0.01), 1)
    F <- round(MSreg / MSres, 2)
    Fcrit <- round(qf(1-alpha, df1, df2), 2)

    question <- paste0("Test the model fit at an $\\alpha$ of ", alpha, ".")

    if (include == TRUE) {

        table <- cbind(X, (X - Xbar)^2, Y, (Y - Ybar)^2, (X - Xbar) * (Y - Ybar), Yhat, (Yhat - Ybar), (Yhat - Ybar)^2)
        table <- round(table, 2)
        colnames(table) <- c("$X_i$",
                             "$(X_i - \\bar{X})^2$",
                             "$Y_i$",
                             "$(Y_i - \\bar{Y})^2$",
                             "$(X_i - \\bar{X})(Y_i - \\bar{Y})$",
                             "$\\hat{Y}_i$",
                             "$(\\hat{Y}_i - \\bar{Y})$",
                             "$(\\hat{Y}_i - \\bar{Y})^2$")
        
        answer <- paste0("
                     \\begin{multicols}{2}
                     \\begin{gather*}
                     \\mathit{SS_X} = ", SSx, " \\\\
                     \\mathit{SP} = ", SP, " \\\\
                     \\beta_1 = ", SP, "/", SSx, " = ", B1, " \\\\
                     \\bar{Y} = ", Ybar, " \\\\
                     \\bar{X} = ", Xbar, " \\\\
                     \\beta_0 = ", Ybar, " - (", B1, " \\times ", Xbar, ") = ", B0, " \\\\
                     \\hat{Y}_i = ", B0, " + (", B1, " \\times X_i ) \\\\
                     \\mathit{SS_{\\textnormal{tot}}} = ", SStot, "
                     \\end{gather*}
                     \\begin{gather*}
                     \\\\
                     \\mathit{SS_{\\textnormal{reg}}} = ", SSreg, " \\\\
                     \\mathit{SS_{\\textnormal{res}}} = ", SStot, " - ", SSreg, " = ", SSres, " \\\\
                     df_1 = ", df1, " \\\\
                     df_2 = ", n, " - ", df1, " - 1 = ", df2, " \\\\
                     \\mathit{MS_{\\textnormal{reg}}} = ", SSreg, " / ", df1, " = ", MSreg, " \\\\
                     \\mathit{MS_{\\textnormal{res}}} = ", SSres, "/", df2, " = ", MSres, " \\\\
                     F = ", MSreg, " / ", MSres, " = ", F, " \\\\
                     F_{\\textnormal{crit}} = ", Fcrit, "
                     \\end{gather*}
                     \\end{multicols}")

        cat(question, capture.output(maketable(table, TRUE)), answer, sep="\n")

    } else {
    
        table <- cbind(X, (X - Xbar)^2, Y, (Y - Ybar)^2, (X - Xbar) * (Y - Ybar))
        table <- round(table, 2)
        colnames(table) <- c("$X_i$",
                             "$(X_i - \\bar{X})^2$",
                             "$Y_i$",
                             "$(Y_i - \\bar{Y})^2$",
                             "$(X_i - \\bar{X})(Y_i - \\bar{Y})$")

        cat(question, capture.output(maketable(table, TRUE)), sep="\n")
    }
}
