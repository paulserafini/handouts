function (percentile, table) {

    n <- max(table$cfreq)

    score <- min(table[table$crfreq >= percentile, 1])
    ll <- score - 0.5
    cumf <- table[table[,1] == score - 1, 3]
    if (length(cumf) == 0) {
        cumf <- 0
    }
    fm <- table[table[,1] == score, 2]
    per <- ll + 1 * ((percentile * n - cumf)/fm)
    per <- round(per, 2)

    cat("\n P_{", 100 * percentile, "} = ", ll, "  + 1 \\times
            [(", percentile, " \\times ", n, " - ", cumf, ") / ", fm, "]
            = ", per, "\n", sep="")
}
