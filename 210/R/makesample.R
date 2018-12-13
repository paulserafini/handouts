function (n) {
    tmp <- 1:3
    while (sum(tmp) %% n != 0) {
        tmp <- runif(n, 0, 10)
        tmp <- round(tmp, 0)
    }
    return(tmp)
}
