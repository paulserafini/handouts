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

    print.xtable(xtable(freq.table, digits=c(0,0,0,0,2,2)),
                 floating=TRUE,
                 table.placement="!h",
                 booktabs=TRUE,
                 include.rownames=FALSE)
    
    if (include.answer == TRUE) {

        median <- median(data)
        ll <- freq.table[freq.table[,1] == median, 1] - 0.5
        cumf <- freq.table[freq.table[,1] == median - 1, 3]
        fm <- freq.table[freq.table[,1] == median, 2]

        answer <- paste0("Median = ", ll, "  + 1
               $\\begin{bmatrix}
               \\frac{0.5(", n, ") - ", cumf, "}{", fm, "}
               \\end{bmatrix}$ = ", round(truemedian(data), 2))

        cat(answer, sep="\n")

    }
}
