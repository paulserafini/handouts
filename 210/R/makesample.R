function (n) {
    tmp <- pi
    while (sum(tmp) %% n > 0) {
        tmp <- sample(0:10, n)
    }
    return(tmp)
}
