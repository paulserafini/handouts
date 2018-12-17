function(include.answer, seed) {

    set.seed(seed)
    n <- 5
    sample <- makesample(n)

    question <- paste(sample, collapse=", ")
    cat(question, "\n")

    if (include.answer) {

        ## Create derivation table
        table <- as.data.frame(sample)
        table$dev <- sample - mean(sample)
        table$devSq <- table$dev^2

        ## Calculate standard deviation
        xbar <- mean(sample)
        SS <- sum(table$devSq)
        df <- n - 1
        var <- SS / df
        sd <- sqrt(var)

        massRound(SS, var, sd)

        colnames(table) <- c("$X_i$",
                             "$X_i - \\bar{X}$",
                             "$(X_i - \\bar{X})^2$")

        table <- xtable(table, digits=0)
        print.xtable(table,
                     floating=TRUE,
                     table.placement="!h",
                     sanitize.text.function=function(x){x},
                     booktabs=TRUE,
                     include.rownames=FALSE)

        cat("\\vspace{-3em}
             \\begin{multicols}{2}
             \\begin{gather*}
             \\bar{X} = ", xbar, " \\\\
             \\mathit{SS} = ", SS, " \\\\
             df = 5 - 1 = ", df, "
             \\end{gather*}
             \\begin{gather*}
             \\\\
             s^2 = ", SS, " / ", df, " = ", var, " \\\\
             s = \\sqrt{", var, "} = ", sd, "
             \\end{gather*}
             \\end{multicols}\n")

    }
}
