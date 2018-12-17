function (...) {

    numbers <- list(...)
    names <- match.call()
    n <- length(numbers)

    for (i in 1:n) {
        name <- names[[i+1]]
        name <- as.character(name)
        value <- numbers[[i]]
        new.value <- round(value, 2)
        assign(name, new.value, envir = parent.frame())
    }

}
