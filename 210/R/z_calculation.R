function(include.answer, seed) {

    ## Create scenario
    set.seed(seed)
    n <- 5
    sample <- makesample(n)
    
    cat("Convert the following scores into $z$ scores.\n")

    if (include.answer) {

        ## Convert scores to z scores
        table <- data.frame(sample)
        xbar <- mean(sample)
        sd <- sd(sample)
        table$dev <- sample - xbar
        table$dev.sq <- table$dev^2
        table$z <- table$dev / sd
        SS <- sum(table$dev.sq)
        var <- SS / (n - 1)
        sd <- sqrt(var)

        massRound(SS, var, sd)

        colnames(table) <- c("$X_i$",
                             "$X_i - \\bar{X}$",
                             "$(X_i - \\bar{X})^2$",
                             "$Z_i$")

        table <- xtable(table, digits=c(0,0,0,0,2))
        print.xtable(table,
                     floating=TRUE,
                     table.placement="!h",
                     sanitize.text.function=function(x){x},
                     booktabs=TRUE,
                     include.rownames=FALSE)

        cat("\\begin{gather*}
             \\bar{X} = ", xbar, " \\\\
             \\mathit{SS} = ", SS, " \\\\
             df = 5 - 1 = 4 \\\\
             s^2 = ", SS, "/4 = ", var, " \\\\
             s = \\sqrt{", var, "} = ", sd, "
             \\end{gather*}\n", sep="")

    } else {

        table <- data.frame(sample)
        colnames(table) <- "$X_i$"
        table <- xtable(table, digits=0)
        print.xtable(table,
                     floating=TRUE,
                     table.placement="!h",
                     sanitize.text.function=function(x){x},
                     booktabs=TRUE,
                     include.rownames=FALSE)

    }
}
