function(include.answer, seed) {

    ## Create scenario
    set.seed(seed)
    data <- rnorm(12, 0, 1)
    data <- round(data, 0)

    question <- paste(data, collapse=", ")
    cat(question, "\n")

    if (include.answer) {

        score <- sort(unique(data))
        freq <- table(data)
        cfreq <- cumsum(freq)
        rfreq <- freq / n
        crfreq <- cumsum(rfreq)
        freq.table <- cbind(score, freq, cfreq, rfreq, crfreq)

        print.xtable(xtable(freq.table, digits=c(0,0,0,0,2,2)),
                     floating=TRUE,
                     table.placement="!h",
                     booktabs=TRUE,
                     include.rownames=FALSE)
}
