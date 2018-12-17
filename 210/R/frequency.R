function(include.answer, seed) {

    ## Create scenario
    set.seed(seed)
    n <- 12
    data <- sample(1:5, 12, replace=TRUE)

    question <- paste(data, collapse=", ")
    cat(question, "\n")

    if (include.answer) {

        score <- sort(unique(data))
        freq <- table(data)
        cfreq <- cumsum(freq)
        rfreq <- freq / n
        crfreq <- cumsum(rfreq)
        freq.table <- cbind(score, freq, cfreq, rfreq, crfreq)

        freq.table <- xtable(freq.table, digits=c(0,0,0,0,2,2))
        print.xtable(freq.table,
                     floating=TRUE,
                     table.placement="!h",
                     booktabs=TRUE,
                     include.rownames=FALSE)
    }
}
