function (include.answer, seed) {

    set.seed(seed)

    ## Create scenario / sample
    mu <- sample(1:10, 1)
    constant <- sample(1:10, 1)
    n <- sample(5:10, 1)
    sigma <- round(runif(1, 1, 5), 2)
    sample <- rtruncnorm(n=n, a=1, b=10, mean=mu, sd=sigma)
    xbar <- round(mean(sample), 2)
    variance <- round(sigma^2, 2)
    
    ## Calculate zcrit
    alpha <- sample(c(0.1, 0.05, 0.01), 1)
    se <- round(sigma / sqrt(n), 2)
    zobs <- round((xbar - constant) / se, 2)
    zcrit <- abs(round(qnorm(alpha/2), 2))
    
    ## Calculate confidence interval
    CC <- sample(c(0.90, 0.95, 0.99), 1)
    CIcrit <- abs(round(qnorm(CC/2), 2))
    LL <- round(xbar - se * CIcrit, 2)
    UL <- round(xbar + se * CIcrit, 2)

    if (2 * pnorm(-abs(zobs)) <= alpha) {
        decision <- ifelse(mu == constant , "false rejection (Type I error)", "correct rejection")
    } else {
        decision <- ifelse(mu == constant , "correct acceptance", "false acceptance (Type II error)")
    }

    answer <- paste0("
                      \\begin{gather*}
                      z_{\\textnormal{crit}} = ", zcrit, " \\\\
                      \\sigma = \\sqrt{", variance, "} = ", sigma, " \\\\
                      \\sigma_{\\bar{X}} = \\frac{", sigma, "}{\\sqrt{", n, "}} = ", se, " \\\\
                      z_{\\textnormal{obs}} = \\frac{", xbar, " - ", constant, "}{", se, "} = ", zobs, " \\\\
                      \\textnormal{They make a ", decision, "} \\\\
                      \\mathit{CI}_{", 100*CC, "} = ", xbar, " \\pm (", se, " \\times ", zcrit, ") = [", LL, " , ", UL, "]
                      \\end{gather*}")
    
    question <- paste0("Researchers draw a sample of ", n, " with a mean of ", xbar, ". The population variance is known to be ", variance, ". Test H_0: \\mu = ", constant, " at an \\alpha of ", alpha, " state the error/decision, and calculate a ", 100*CC, "% confidence interval. The population mean is actually ", mu, ".", sep = "")

    if (include.answer == TRUE) {
        cat(question, answer, sep="\n")
    } else {
        cat(question, sep="\n")
    }
}
