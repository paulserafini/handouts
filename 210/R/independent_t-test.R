function (include.answer, seed) {

    set.seed(seed)
    alpha <- sample(c(0.1, 0.05, 0.01), 1)
    CC <- sample(c(0.90, 0.95, 0.99), 1)

    ## Create sample 1
    n1 <- sample(6:7, 1)
    sample1 <- sample(1:10, n1)
    var1 <- var(sample1)
    xbar1 <- mean(sample1)

    ## Create sample 2
    n2 <- sample(6:7, 1)
    sample2 <- sample(1:10, n2)
    var2 <- var(sample2)
    xbar2 <- mean(sample2)

    massRound(var1, xbar1, var2, xbar2)

    ## Print question
    cat("Researchers draw one sample of ", n1, "
         with a mean of ", xbar1, " and a variance of ", var1, ",
         and another sample of ", n2, " with a mean of ", xbar2, "
         and a variance of ", var2, ".
         Test $H_0: \\mu_1 = \\mu_2$ at an \\alpha of ", alpha, ",
         state the error, then calculate a ", 100*CC, "% confidence interval.\n", sep="")

    if (include.answer) {

        ## Sample 1 derivatives
        df1 <- n1 - 1
        SS1 <- var1 * df1

        ## Sample 2 derivatives
        df2 <- n2 - 1
        SS2 <- var2 * df2

        ## Calculate tobs
        dftot <- df1 + df2
        var.p <- (SS1 + SS2) / dftot
        se <- sqrt((var.p / n1) + (var.p / n2))
        tobs <- (xbar1 - xbar2) / se

        tcrit <- qt(alpha / 2, dftot)
        tcrit <- abs(tcrit)

        ## Calculate confidence interval
        CIcrit <- qt((1 - CC)/2, dftot)
        CIcrit <- abs(CIcrit)
        LL <- (xbar1 - xbar2) - (se * CIcrit)
        UL <- (xbar1 - xbar2) + (se * CIcrit)

        massRound(SS1, SS2, var.p, se, tobs, tcrit, CIcrit, LL, UL)

        if (abs(tobs) >= tcrit) {
            operator <- ifelse(tobs >= tcrit, ">= ", " =< -")
            decision <- paste0("Reject because $", tobs, operator, tcrit, "$")
        } else {
            decision <- paste0("Fail to reject because $", tcrit, " > ", tobs, " > -", tcrit, "$")
        }

        cat("\\begin{gather*}
             df_1 = ", n1, " - 1 = ", df1, " \\\\
             df_2 = ", n2, " - 1 = ", df2, " \\\\
             \\mathit{SS}_1 = ", var1, " \\times ", df1, " = ", SS1, " \\\\
             \\mathit{SS}_2 = ", var2, " \\times ", df2, " = ", SS2, " \\\\
             df_{\\textnormal{tot}} = ", df1, " + ", df2, " = ", dftot, " \\\\
             s^2_p = (", SS1, " + ", SS2, ") / ", dftot, " = ", var.p, " \\\\
             s_{(\\bar{X}_1 - \\bar{X}_2)} = \\sqrt{(", var.p, "/", n1, ") + (", var.p, "/", n2, ")} = ", se, " \\\\
             t_{\\textnormal{obs}}(", dftot, ") = (", xbar1, " - ", xbar2, ") / ", se, " = ", tobs, " \\\\
             t_{\\textnormal{crit}} = \\pm", tcrit, "\\\\
             \\textnormal{", decision, "} \\\\
             t_{", 100*CC, "} = ", CIcrit, "\\\\
             \\mathit{CI}_{", 100*CC, "} = (", xbar1, " - ", xbar2, ") \\pm (", se, " \\times ", CIcrit, ") = [", LL, ",\\ ", UL, "]
             \\end{gather*}\n", sep="")

    }
}
