function (include.answer, seed) {

    n <- 12

    mean <- sample(5:15, 1)
    data <- round(rnorm(n, mean, 1), 0)

    ## Find a sample where the complicated procedure is necessary
    while (truemedian(data) == median(data) || median(data) %% 1 > 0) {
        mean <- sample(5:15, 1)
        data <- round(rnorm(n, mean, 1), 0)
    }

    score <- sort(unique(data))
    freq <- table(data)
    cfreq <- cumsum(freq)
    rfreq <- freq / n
    crfreq <- cumsum(rfreq)
    freq.table <- cbind(score, freq, cfreq, rfreq, crfreq)
    freq.table <- apply(freq.table, 2, rev)
    freq.table <- as.data.frame(freq.table)

    freq.table <- xtable(freq.table, digits=c(0,0,0,0,2,2))
    print.xtable(freq.table,
                 floating=TRUE,
                 table.placement="!h",
                 booktabs=TRUE,
                 include.rownames=FALSE)
    
    if (include.answer) {


        perCalc(0.25, freq.table)
        perCalc(0.50, freq.table)
        perCalc(0.75, freq.table)
        
    }
}
