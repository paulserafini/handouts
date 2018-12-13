function (include.answer, seed) {

    set.seed(seed)

    ## Create scenario / sample
    mu <- sample(1:10, 1)
    n <- sample(5:7, 1)
    sigma <- round(runif(1, 1, 5), 2)
    sample <- rtruncnorm(n=n, a=1, b=10, mean=mu, sd=sigma)
    constant <- sample(1:10, 1)
    sd <- round(sd(sample), 2)
    xbar <- round(mean(sample), 2)

    ## Calculate tcrit
    alpha <- sample(c(0.1, 0.05, 0.01), 1)
    df <- n - 1
    se <- round(sd / sqrt(n), 2)
    tobs <- round((xbar - constant) / se, 2)
    tcrit <- abs(round(qt(alpha/2, df), 2))

    ## Calculate confidence interval
    CC <- sample(c(0.90, 0.95, 0.99), 1)
    CIcrit <- abs(round(qt((1 - CC)/2, df), 2))
    LL <- round(xbar - se * CIcrit, 2)
    UL <- round(xbar + se * CIcrit, 2)

    if (2 * pt(-abs(tobs),df=df) <= alpha) {
        error <- ifelse(mu == constant , "false rejection (type I error)", "correct rejection")
    } else {
        error <- ifelse(mu == constant , "correct acceptance", "false acceptance (type II error)")
    }

    answer <- paste0("
                      \\begin{gather*}
                      s_{\\bar{X}} = \\frac{", sd, "}{ \\sqrt{", n, "}} = ", se, " \\\\
                      t_{\\textnormal{tobs}} = (", xbar, " - ", constant, ") / ", se, " = ", tobs, " \\\\
                      t_{\\textnormal{crit}} = ", tcrit, "\\\\
                      \\textnormal{They make a ", error, "} \\\\
                      \\mathit{CI}_{", 100*CC, "} = ", xbar, " \\pm (", se, " \\times ", CIcrit, ") = [", LL, " , ", UL, "]
                      \\end{gather*}")

    question <- paste0("Researchers draw a sample of ", n, " with a mean of ", xbar, " and a standard deviation of ", sd, ". Test H_0: \\mu = ", constant, " at an \\alpha of ", alpha, ", state the error/decision, then calculate a ", 100*CC, "% confidence interval. In reality, \\mu = ", mu, ".", sep="")

    if (include.answer == TRUE) {
        cat(question, answer, sep="\n")
    } else {
        cat(question, sep="\n")
    }
}
