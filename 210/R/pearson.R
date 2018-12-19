function (include.answer, seed) {

    ## Create scenario
    set.seed(seed)
    n <- sample(4:6, 1)
    X <- makesample(n)
    Y <- makesample(n)
    alpha <- sample(c(0.2, 0.1), 1)

    ## Start derivation table
    table <- cbind(X, Y)
    table <- as.data.frame(table)

    cat("Calculate $r_{\\mathit{XY}}$ and test $H_0: \\rho_{\\mathit{XY}} = 0$ at $\\alpha = ", alpha, "$.\n", sep="")

    if (include.answer) {

        ## X derivatives
        Xsd <- sd(X)
        Xbar <- mean(X)

        ## Y derivatives
        Ysd <- sd(Y)
        Ybar <- mean(Y)

        ## Finish derivation table table
        table$Xdev <- X - Xbar
        table$Ydev <- Y - Ybar
        table$Xdev.sq <- table$Xdev^2
        table$Ydev.sq <- table$Ydev^2
        table$SP <- table$Xdev * table$Ydev

        ## Calculate r
        SSx <- sum(table$Xdev.sq)
        SSy <- sum(table$Ydev.sq)
        SP <- sum(table$SP)
        r <- SP / sqrt(SSx * SSy)

        ## Make a decision
        rcrit <- rcrit(n, alpha)
        significant <- abs(r) >= rcrit

        massRound(Xsd, Ysd, SSx, SSy, SP, r, rcrit)

        colnames(table) <- c("$X_i$",
                             "$Y_i$",
                             "$X_i - \\bar{X}$",
                             "$Y_i - \\bar{Y}$",
                             "$(X_i - \\bar{X})^2$",
                             "$(Y_i - \\bar{Y})^2$",
                             "$(X_i - \\bar{X})(Y_i - \\bar{Y})$")

        table <- xtable(table, digits=0)
        print.xtable(table,
                     floating=TRUE,
                     table.placement="!h",
                     sanitize.text.function=function(x){x},
                     booktabs=TRUE,
                     include.rownames=FALSE)

        if (significant) {
            operator <- ifelse(robs > rcrit, ">= ", " <= -")
            decision <- paste0("Reject because $", r, operator, rcrit, "$")
        } else {
            decision <- paste0("Fail to reject because $", rcrit, " > ", r, " > -", rcrit, "$")
        }

        cat("\\vspace{-3em}
             \\begin{multicols}{2}
             \\begin{gather*}
             \\bar{X} = ", Xbar, " \\\\
             \\bar{Y} = ", Ybar, " \\\\
             \\mathit{SS_X} = ", SSx, " \\\\
             \\mathit{SS_Y} = ", SSy, "
             \\end{gather*}
             \\begin{gather*}
             \\\\
             \\mathit{SP} = ", SP, " \\\\
             r_{\\mathit{XY}} = ", SP, " / \\sqrt{", SSx, " \\times ", SSy, "} = ", r, " \\\\
             r_{\\textnormal{crit}} = \\pm", rcrit, " \\\\
             \\textnormal{", decision, "}
             \\end{gather*}
             \\end{multicols}\n", sep="")

    } else {

        cat("\n\\begin{minipage}[t][4cm][t]{3cm} \\vspace{0.25cm}\n")

        colnames(table) <- c("$X_i$", "$Y_i$")
        table <- xtable(table, digits=0)
        print.xtable(table,
                     floating=FALSE,
                     sanitize.text.function=function(x){x},
                     booktabs=TRUE,
                     include.rownames=FALSE)

        cat("\n\\end{minipage}\n")
        
    }
}
