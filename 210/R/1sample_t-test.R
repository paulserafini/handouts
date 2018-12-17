function (include.answer, seed) {

    set.seed(seed)

    n <- sample(5:7, 1)
    sample <- sample(1:10, n)
    sd <- sd(sample)
    xbar <- mean(sample)
    massRound(sd, xbar)

    alpha <- sample(c(0.1, 0.05, 0.01), 1)
    CC <- sample(c(0.90, 0.95, 0.99), 1)
    mu <- sample(1:10, 1)

    question <- paste0("Researchers draw a sample of ", n, " with a mean of ", xbar, " and a standard deviation of ", sd, ".
                        Test $H_0: \\mu = ", mu, "$ at an \\alpha of ", alpha, ",
                        state your decision, then calculate a ", 100*CC, "% confidence interval.", sep="")
    cat(question, sep="\n")

    if (include.answer == TRUE) {

        df <- n - 1
        se <- sd / sqrt(n)
        tobs <- (xbar - mu) / se

        tcrit <- qt(alpha/2, df)
        tcrit <- abs(tcrit)

        CIcrit <- qt((1 - CC)/2, df)
        CIcrit <- abs(CIcrit)

        LL <- xbar - se * CIcrit
        UL <- xbar + se * CIcrit

        massRound(se, tobs, tcrit, CIcrit, LL, UL)

        if (abs(tcrit) > tobs) {
            operator <- ifelse(tobs > tcrit, ">= ", " =< -")
            decision <- paste0("Reject because $", tobs, operator, tcrit, "$")
        } else {
            decision <- paste0("Fail to reject because $", tcrit, " > ", tobs, " > -", tcrit, "$")
        }

        answer <- paste0("\\begin{gather*}
                      s_{\\bar{X}} = ", sd, "/\\sqrt{", n, "} = ", se, " \\\\
                      t_{\\textnormal{tobs}} = (", xbar, " - ", mu, ") / ", se, " = ", tobs, " \\\\
                      t_{\\textnormal{crit}} = \\pm", tcrit, "\\\\
                      \\textnormal{", decision, "} \\\\
                      t_{", 100*CC, "} = ", CIcrit, "\\\\
                      \\mathit{CI}_{", 100*CC, "} = ", xbar, " \\pm (", se, " \\times ", CIcrit, ") = [", LL, " ,\\ ", UL, "]
                      \\end{gather*}")

        cat(answer, sep="\n")

    }
}
