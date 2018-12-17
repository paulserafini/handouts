function (include.answer, seed) {

    ## Create scenario
    set.seed(seed)
    n <- sample(5:7, 1)
    sample1 <- sample(1:10, n)
    sample2 <- sample(1:10, n)
    CC <- sample(c(0.90, 0.95, 0.99), 1)
    alpha <- sample(c(0.1, 0.05, 0.01), 1)
   
    ## Begin derivation table
    table <- cbind(sample1, sample2)
    table <- as.data.frame(table)
    
    cat("Test $H_0: \\mu_{\\bar{D}} = 0$ at an \\alpha of ", alpha, ",
         state the decision/error, then calculate a ", 100*CC, "% confidence interval.\n", sep="")

    if (include.answer) {

        ## Complete derivation table
        table$D <- sample1 - sample2
        Dbar <- mean(table$D)
        table$dev <- table$D - Dbar
        table$devSq <- table$dev^2

        ## Calculate tcrit
        df <- n - 1
        sumDevSq <- sum(table$devSq)
        sD <- sqrt(sumDevSq / df)
        sDbar <- sD / sqrt(n)
        tobs <- Dbar / sDbar
        tcrit <- qt(alpha/2, df)
        tcrit <- abs(tcrit)

        ## Calculate confidence interval
        CIcrit <- (1 - CC)/2
        CIcrit <- qt(CIcrit, df)
        CIcrit <- abs(CIcrit)
        LL <- (Dbar - sDbar * CIcrit)
        UL <- (Dbar + sDbar * CIcrit)

        massRound(Dbar, sumDevSq, sD, sDbar, tobs, tcrit, CIcrit, LL, UL)

        colnames(table) <- c("Pre",
                             "Post",
                             "$D_i$",
                             "$D_i - \\bar{D}$",
                             "$(D_i - \\bar{D})^2$")

        table <- xtable(table,
                        digits=c(0,0,0,0,2,2))

        print.xtable(table,
                     floating=TRUE,
                     table.placement="!h",
                     sanitize.text.function=function(x){x},
                     booktabs=TRUE,
                     include.rownames=FALSE)

        if (abs(tobs) >= tcrit) {
            operator <- ifelse(tobs > tcrit, ">= ", " =< -")
            decision <- paste0("Reject because $", tobs, operator, tcrit, "$")
        } else {
            decision <- paste0("Fail to reject because $", tcrit, " > ", tobs, " > -", tcrit, "$")
        }

        cat("\\begin{gather*}
             \\bar{D} = ", Dbar, " \\\\
             \\Sigma (D_i - \\bar{D})^2 = ", sumDevSq, " \\\\
             df = ", df, " \\\\
             s_D = \\sqrt{", sumDevSq, " / ", df, "} = ", sD, " \\\\
             s_{\\bar{D}} = ", sD, " / \\sqrt{", n, "} = ", sDbar, " \\\\
             t_{\\textnormal{obs}}(", df, ") = ", Dbar, " / ", sDbar, " = ", tobs, " \\\\
             t_{\\textnormal{crit}} = ", tcrit, " \\\\
             \\textnormal{", decision, "} \\\\
             t_{", 100*CC, "} = ", CIcrit, " \\\\
             \\mathit{CI}_{", 100*CC, "} = ", Dbar,  " \\pm (", sDbar, " \\times ", CIcrit, ") = [", LL, ",\\ ", UL, "]
             \\end{gather*}\n", sep="")

    } else {

        colnames(table) <- c("Pre", "Post")
        table <- xtable(table, digits=0)
        print.xtable(table,
                     floating=TRUE,
                     table.placement="!h",
                     sanitize.text.function=function(x){x},
                     booktabs=TRUE,
                     include.rownames=FALSE)

    }
}
