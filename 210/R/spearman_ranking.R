function(include.answer, seed) {

    set.seed(seed)
    sample <- sample(1:9, 4, replace=TRUE)
    sample <- c(sample, sample[1])

    question <- paste(sample, collapse=", ")
    cat(question, "\n")

    if (include.answer) {

        ## Create derivation table
        table <- as.data.frame(sample)
        table$sorted <- sort(sample)
        table$rank1 <- 1:5
        table$rank2 <- rank(table$sorted, ties.method="average")
    
        colnames(table) <- c("$X_i$",
                             "Sorted",
                             "Ranking 1",
                             "Ranking 2")

        table <- xtable(table, digits=c(0,0,0,0,1))
        print.xtable(table,
                     floating=TRUE,
                     table.placement="!h",
                     sanitize.text.function=function(x){x},
                     booktabs=TRUE,
                     include.rownames=FALSE)

    }
}
