function (include.answer, seed) {

    set.seed(seed)

    ## Create sample 1
    mu1 <- sample(1:10, 1)
    n1 <- sample(6:7, 1)
    df1 <- n1 - 1
    sigma1 <- round(runif(1, 1, 5), 2)
    sample1 <- rtruncnorm(n=n1, a=1, b=10, mean=mu1, sd=sigma1)
    var1 <- round(var(sample1), 2)
    xbar1 <- round(mean(sample1), 2)
    SS1 <- var1 * df1
    sd1 <- round(sqrt(var1), 2)

    ## Create sample 2
    mu2 <- sample(1:10, 1)
    n2 <- ifelse(n1 %% 3 == 0, n1, sample(6:7, 1)) ## Make sure they're equal sometimes
    df2 <- n2 - 1
    sigma2 <- runif(1, 1, 3)
    sample2 <- rtruncnorm(n=n2, a=1, b=10, mean=mu2, sd=sigma2)
    var2 <- round(var(sample2), 2)
    xbar2 <- round(mean(sample2), 2)
    SS2 <- var2 * df2
    sd2 <- round(sqrt(var2), 2)

    ## Calculate tcrit
    alpha <- sample(c(0.1, 0.05, 0.01), 1)
    var.p <- round((SS1 + SS2) / (n1 + n2 - 2), 2)
    se <- round(sqrt((var.p/n1) + (var.p/n2)), 2)
    tobs <- round((xbar1 - xbar2) / se, 2)
    dftot <- n1 + n2 - 2
    tcrit <- abs(round(qt(alpha/2, dftot), 2))

    ## Calculate confidence interval
    CC <- sample(c(0.90, 0.95, 0.99), 1)
    CIcrit <- abs(round(qt((1 - CC)/2, dftot), 2))
    LL <- round((xbar1 - xbar2) - se * CIcrit, 2)
    UL <- round((xbar1 - xbar2) + se * CIcrit, 2)

    if (2 * pt(-abs(tobs),df=dftot) <= alpha) {
        decision <- ifelse(mu1 == mu2 , "false rejection (type I error)", "correct rejection")
    } else {
        decision <- ifelse(mu1 == mu2 , "correct acceptance", "false acceptance (type II error)")
    }

    question <- paste0("Researchers draw one sample of ", n1, " with a mean of ", xbar1, " and a standard deviation of ", sd1, ", and another sample of ", n2, " with a mean of ", xbar2, " and a standard deviation of ", sd2, ". Test H_0: \\mu_1 = \\mu_2 at an \\alpha of ", alpha, ", state the decision/error, then calculate a ", 100*CC, "% confidence interval. In reality, \\mu_1 = ", mu1, " and \\mu_2 = ", mu2, ".")
    
    if (include.answer == TRUE) {
    
        answer <- paste0("
                          \\begin{gather*}
                          s^2_1 = ", sd1, "^2 = ", var1, " \\\\
                          s^2_2 = ", sd2, "^2 = ", var2, " \\\\
                          df_1 = ", n1, " - 1 = ", df1, " \\\\
                          df_2 = ", n2, " - 1 = ", df2, " \\\\
                          \\mathit{SS}_1 = ", var1, " \\times ", df1, " = ", SS1, " \\\\
                          \\mathit{SS}_2 = ", var2, " \\times ", df2, " = ", SS2, " \\\\
                          df_{\\textnormal{tot}} = ", df1, " + ", df2, " = ", dftot, " \\\\
                          s^2_p = (", SS1, " + ", SS2, ") / ", dftot, " = ", var.p, " \\\\
                          s_{(\\bar{X}_1 - \\bar{X}_2)} = \\sqrt{(", var.p, "/", n1, ") + (", var.p, "/", n2, ")} = ", se, " \\\\
                          t_{\\textnormal{obs}}(", dftot, ") = (", xbar1, " - ", xbar2, ") / ", se, " = ", tobs, " \\\\
                          t_{\\textnormal{crit}} = \\pm", tcrit, "\\\\
                          \\textnormal{They make a ", decision, "} \\\\
                          \\mathit{CI}_{", 100*CC, "} = (", xbar1, " - ", xbar2, ") \\pm ", se, " \\times ", CIcrit, " = [", LL, ", ", UL, "]
                          \\end{gather*}")

        cat(question, answer, sep="\n")

    } else {

        cat(question, sep="\n")

    }
}
