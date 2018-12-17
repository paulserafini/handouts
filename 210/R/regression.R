function (include.answer, seed) {

    set.seed(seed)
    n <- sample(4:6, 1)
    alpha <- sample(c(0.05, 0.01), 1)

    ## X derivations
    X <- makesample(n)
    Xbar <- mean(X)

    ## Y derivations
    Y <- makesample(n)
    Ybar <- mean(Y)

    ## Begin table
    table <- cbind(X, Y)
    table <- as.data.frame(table)
    table$XdevSq <- (X - Xbar)^2
    table$YdevSq <- (Y - Ybar)^2
    table$products <- (X - Xbar) * (Y - Ybar)

    cat("Test the model fit at an $\\alpha$ of ", alpha, ".\n", sep="")

    if (include.answer) {

        ## Calculate regression formula
        SP <- sum(table$products)
        SSx <- sum(table$XdevSq)
        SSy <- sum(table$YdevSq)
        B1 <- SP / SSx
        B0 <- Ybar - B1 * Xbar

        ## Complete table
        table$Yhat <- B0 + X * B1
        table$Res <- table$Yhat - Ybar
        table$ResSq <- table$Res^2
        table <- round(table, 2)

        ## Calculate Fcrit
        SStot <- sum(table$YdevSq)
        SSreg <- sum(table$ResSq)
        SSres <- SStot - SSreg
        df1 <- 1
        df2 <- n - df1 - 1
        MSreg <- SSreg / df1
        MSres <- SSres / df2
        F <- MSreg / MSres
        Fcrit <- qf(1-alpha, df1, df2)

        massRound(SSx, SSy, SP, B1, B0, SStot, SSreg, SSres, MSreg, MSres, F, Fcrit)

        if (F >= Fcrit) {
            decision <- paste0("Reject because $", F, " >= ", Fcrit, "$")
        } else {
            decision <- paste0("Fail to reject because $", F, " < ", Fcrit, "$")
        }

        colnames(table) <- c("$X_i$",
                             "$Y_i$",
                             "$(X_i - \\bar{X})^2$",
                             "$(Y_i - \\bar{Y})^2$",
                             "$(X_i - \\bar{X})(Y_i - \\bar{Y})$",
                             "$\\hat{Y}_i$",
                             "$(\\hat{Y}_i - \\bar{Y})$",
                             "$(\\hat{Y}_i - \\bar{Y})^2$")

        table <- xtable(table, digits=c(0,0,0,0,0,0,2,2,2))
        print.xtable(table,
                     floating=TRUE,
                     table.placement="!h",
                     sanitize.text.function=function(x){x},
                     booktabs=TRUE,
                     include.rownames=FALSE)

        cat("\\begin{multicols}{2}
             \\begin{gather*}
             \\mathit{SS_X} = ", SSx, " \\\\
             \\mathit{SP} = ", SP, " \\\\
             \\beta_1 = ", SP, "/", SSx, " = ", B1, " \\\\
             \\bar{Y} = ", Ybar, " \\\\
             \\bar{X} = ", Xbar, " \\\\
             \\beta_0 = ", Ybar, " - (", B1, " \\times ", Xbar, ") = ", B0, " \\\\
             \\hat{Y}_i = ", B0, " + (", B1, " \\times X_i ) \\\\
             \\mathit{SS_{\\textnormal{tot}}} = ", SStot, " \\\\
             \\mathit{SS_{\\textnormal{reg}}} = ", SSreg, " \\\\
             \\end{gather*}
             \\begin{gather*}
             \\\\
             \\mathit{SS_{\\textnormal{res}}} = ", SStot, " - ", SSreg, " = ", SSres, " \\\\
             df_1 = ", df1, " \\\\
             df_2 = ", n, " - ", df1, " - 1 = ", df2, " \\\\
             \\mathit{MS_{\\textnormal{reg}}} = ", SSreg, " / ", df1, " = ", MSreg, " \\\\
             \\mathit{MS_{\\textnormal{res}}} = ", SSres, "/", df2, " = ", MSres, " \\\\
             F = ", MSreg, " / ", MSres, " = ", F, " \\\\
             F_{\\textnormal{crit}} = ", Fcrit, " \\\\
             \\textnormal{", decision, "}
             \\end{gather*}
             \\end{multicols}\n", sep="")

    } else {

        cat("\n\\begin{minipage}[t][4cm][t]{6cm} \\vspace{0.25cm}\n")

        colnames(table) <- c("$X_i$",
                             "$Y_i$",
                             "$(X_i - \\bar{X})^2$",
                             "$(Y_i - \\bar{Y})^2$",
                             "$(X_i - \\bar{X})(Y_i - \\bar{Y})$")

        table <- xtable(table, digits=c(0,0,0,0,0,0))
        print.xtable(table,
                     floating=FALSE,
                     sanitize.text.function=function(x){x},
                     booktabs=TRUE,
                     include.rownames=FALSE)

        cat("\n\\end{minipage}\n")

    }

}
