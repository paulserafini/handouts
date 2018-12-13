function(include.answer, seed) {

    set.seed(seed)

    rnorm(12, 0, 1) %>>%
        round(0) %>>%
        (~ data)

    if (include.answer == TRUE) {

        paste(data, collapse=", ") %>>%
            cat()

        data %>>%
            plyr::count() %>>%
            mutate(cfreq = cumsum(freq)) %>>%
            mutate(rfreq = freq / 12) %>>%
            mutate(crfreq = cumsum(rfreq)) %>>%
            round(2) %>>%
            rename(score=x) %>>%
            xtable(digits=c(0,0,0,0,2,2)) %>>%
            print.xtable(floating=TRUE,
                         table.placement="!h",
                         booktabs=TRUE,
                         include.rownames=FALSE)

    } else {

        paste(data, collapse=", ") %>>%
            cat("\n")

    }

}
