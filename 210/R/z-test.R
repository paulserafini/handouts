function (include.answer, seed) {

    ## Create scenario
    set.seed(seed)
    n <- sample(5:10, 1)
    sigma <- runif(1, 1, 5)
    mu <- sample(1:10, 1)
    sample <- sample(1:10, n, replace=TRUE)
    xbar <- mean(sample)
    alpha <- sample(c(0.1, 0.05, 0.01), 1)
    CC <- sample(1-alpha, 1)

    massRound(xbar, sigma)

    cat("Researchers draw a sample of ", n, " with a mean of ", xbar, ".
         The population standard deviation is known to be ", sigma, ".
         Test $H_0: \\mu = ", mu, "$ at an \\alpha of ", alpha, ",
         state your decision, and calculate a ", 100*CC, "% confidence interval.\n", sep = "")

    if (include.answer) {
        
        ## Calculate zobs
        se <- sigma / sqrt(n)
        zobs <- (xbar - mu) / se

        ## Make a decision
        zcrit <- qnorm(alpha / 2)
        zcrit <- abs(zcrit)
        significant <- abs(zobs) >= zcrit 

        ## Calculate confidence interval
        CIcrit <- qnorm((1 - CC) / 2)
        CIcrit <- abs(CIcrit)
        LL <- xbar - (se * CIcrit)
        UL <- xbar + (se * CIcrit)

        massRound(se, zobs, zcrit, CIcrit, LL, UL)

        if (significant) {
            operator <- ifelse(zobs >= zcrit, ">= ", " <= -")
            decision <- paste0("Reject because $", zobs, operator, zcrit, "$")
        } else {
            decision <- paste0("Fail to reject because $", zcrit, " > ", zobs, " > -", zcrit, "$")
        }

        cat("\\begin{gather*}
             \\sigma_{\\bar{X}} = ", sigma, "/\\sqrt{", n, "} = ", se, " \\\\
             z_{\\textnormal{obs}} = (", xbar, " - ", mu, ")/", se, " = ", zobs, " \\\\
             z_{\\textnormal{crit}} = \\pm", zcrit, " \\\\
             \\textnormal{", decision, "} \\\\
             z_{", 100*CC, "} = ", CIcrit, " \\\\
             \\mathit{CI}_{", 100*CC, "} = ", xbar, " \\pm (", se, " \\times ", CIcrit, ") = [", LL, " ,\\ ", UL, "]
             \\end{gather*}\n", sep="")

    }
}
