function(include.answer, seed) {

    set.seed(seed)

    data <- makesample(5)

    paste(data, collapse=", ") %>>% cat(sep="\n")

    if (include.answer == TRUE) {

        data %>>%
            as.data.frame() %>>%
            rename(xi = 1) %>>%
            mutate(dev = xi - mean(xi)) %>>%
            mutate(dev.sq = dev^2) %>>%
            (~ tmp) %>>%
            rename("$X_i$"=xi,
                   "$X_i - \\bar{X}$"=dev,
                   "$(X_i - \\bar{X})^2$"=dev.sq) %>>%
            xtable(digits=c(0,0,0,0)) %>>%
            print.xtable(floating=TRUE,
                         table.placement="!h",
                         sanitize.text.function=function(x){x},
                         booktabs=TRUE,
                         include.rownames=FALSE)

        xbar <- mean(tmp$xi)
        SS <- sum(tmp$dev.sq)
        df <- 4
        var <- round(SS / df, 2)
        sd <- round(sqrt(var), 2)

        paste0("
                \\vspace{-3em}
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
            \\end{multicols}") %>% cat(sep="\n")

    }

}
