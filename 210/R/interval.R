function(include.answer, seed) {

    ## Create scenario
    set.seed(seed)
    width <- sample(1:50, 1)
    lowest <- sample(1:100, 1)

    ## Create question
    question <- paste0("The lowest value in the data set is ", lowest, ",
                        and the desired interval width is ", width, ".")
    cat(question, "\n")

    if (include.answer) {

        ## Find LL1 and LL5
        multiples <- seq(from = 0, to = lowest, by = width)
        LL1 <- max(multiples)
        LL5 <- LL1 + width * 4

        ## Derive LL, MP, and UL of first five intervals
        LL <- seq(from = LL1, to = LL5, by = width)
        UL <- LL + width - 1
        MP <- (LL - 0.5) + (width / 2)
        table <- cbind(LL, MP, UL)

        table <- xtable(table, digits=c(0,0,1,0))
        print.xtable(table,
                     floating=TRUE,
                     table.placement="!h",
                     booktabs=TRUE,
                     include.rownames=FALSE)
        
    }
}
