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

    ## Start derivation table
    table <- cbind(X, Y)
    table <- as.data.frame(table)
    table$XdevSq <- (X - Xbar)^2
    table$products <- (X - Xbar) * (Y - Ybar)

    newX <- sample(20:100, 5)

    cat("Calculate the regression equation for predicting Y from X, and calculate the predicted Y score for each X score. Then, calculate the predicted score for X_i = ", paste(newX, collapse=", "), ".\n", sep="")

    if (include.answer) {

        ## Calculate regression formula
        SP <- sum(table$products)
        SSx <- sum(table$XdevSq)
        B1 <- SP / SSx
        B0 <- Ybar - B1 * Xbar

        ## Apply regression formula
        table$Yhat <- B0 + X * B1

        massRound(SSx, SP, B1, B0)

        colnames(table) <- c("$X_i$",
                             "$Y_i$",
                             "$(X_i - \\bar{X})^2$",
                             "$(X_i - \\bar{X})(Y_i - \\bar{Y})$",
                             "$\\hat{Y}_i$")

        table <- xtable(table, digits=c(0,0,0,0,0,2))
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
             \\end{gather*}
             \\begin{gather*}
             \\\\
             \\hat{Y}_{", newX[1], "} = ", B0, " + (", B1, " \\times ", newX[1], ") = ", B0 + (B1 * newX[1]), " \\\\
             \\hat{Y}_{", newX[2], "} = ", B0, " + (", B1, " \\times ", newX[2], ") = ", B0 + (B1 * newX[2]), " \\\\
             \\hat{Y}_{", newX[3], "} = ", B0, " + (", B1, " \\times ", newX[3], ") = ", B0 + (B1 * newX[3]), " \\\\
             \\hat{Y}_{", newX[4], "} = ", B0, " + (", B1, " \\times ", newX[4], ") = ", B0 + (B1 * newX[4]), " \\\\
             \\hat{Y}_{", newX[5], "} = ", B0, " + (", B1, " \\times ", newX[5], ") = ", B0 + (B1 * newX[5]), " \\\\
             \\\\
             \\end{gather*}
             \\end{multicols}\n", sep="")

    } else {

        cat("\n\\begin{minipage}[t][4cm][t]{6cm} \\vspace{0.25cm}\n")

        colnames(table) <- c("$X_i$",
                             "$Y_i$",
                             "$(X_i - \\bar{X})^2$",
                             "$(Y_i - \\bar{Y})^2$",
                             "$(X_i - \\bar{X})(Y_i - \\bar{Y})$")

        table <- xtable(table, digits=0)
        print.xtable(table,
                     floating=FALSE,
                     sanitize.text.function=function(x){x},
                     booktabs=TRUE,
                     include.rownames=FALSE)

        cat("\n\\end{minipage}\n")

    }

}
