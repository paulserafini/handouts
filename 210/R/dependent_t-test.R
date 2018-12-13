function (include.answer, seed) {

    set.seed(seed)

    n <- round(runif(1, 5, 7), 0)

    ## Create sample 1
    mu1 <- sample(1:10, 1)
    sigma1 <- round(runif(1, 1, 5), 2)
    sample1 <- round(rtruncnorm(n=n, a=1, b=10, mean=mu1, sd=sigma1), 0)
    xbar1 <- round(mean(sample1), 2)

    ## Create sample 2
    mu2 <- sample(1:10, 1)
    sigma2 <- runif(1, 1, 3)
    sample2 <- round(rtruncnorm(n=n, a=1, b=10, mean=mu2, sd=sigma2), 0)
    xbar2 <- round(mean(sample2), 2)

    ## Calculate tcrit
    df <- n - 1
    alpha <- sample(c(0.1, 0.05, 0.01), 1)
    D <- sample1 - sample2
    Dbar <- round(mean(D), 2)
    dev <- D - Dbar
    devSq <- round(dev^2, 2)
    sumDevSq <- sum(devSq)
    sD <- round(sqrt(sumDevSq / df), 2)
    sDbar <- round(sD / sqrt(n), 2)
    tobs <- round(Dbar / sDbar, 2)
    tcrit <- abs(round(qt(alpha/2, df), 2))

    ## Calculate confidence interval
    CC <- sample(c(0.90, 0.95, 0.99), 1)
    CIcrit <- abs(round(qt((1 - CC)/2, df), 2))
    LL <- round(Dbar - sDbar * CIcrit, 2)
    UL <- round(Dbar + sDbar * CIcrit, 2)

    if (2 * pt(-abs(tobs),df=df) <= alpha) {
        operator <- ifelse(tobs > tcrit, ">= ", " =< -")
        decision <- paste0("Reject because $", tobs, operator, tcrit, "$")
    } else {
        decision <- paste0("Fail to reject because $", tcrit, " > ", tobs, " > -", tcrit, "$")
    }
    
    answer <- paste0("
                      \\begin{gather*}
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
                      \\end{gather*}")
                      
    question1 <- "Researchers collect the following data:"
    
    question2 <- paste0("Test $H_0: \\mu_{\\bar{D}} = 0$ at an \\alpha of ", alpha, ", state the decision/error, then calculate a ", 100*CC, "% confidence interval.")

    if (include.answer == TRUE) {

        table <- cbind(sample1, sample2, D, dev, devSq)
        colnames(table) <- c("Pre", "Post", "$D_i$", "$D_i - \\bar{D}$", "$(D_i - \\bar{D})^2$")
        table <- capture.output(maketable(table, TRUE))

        cat(question1, table, question2, answer, sep="\n")

    } else {

        table <- cbind(sample1, sample2)
        colnames(table) <- c("Pre", "Post")
        table <- capture.output(maketable(table, TRUE))

        cat(question1, table, question2, sep="\n")

    }
}
