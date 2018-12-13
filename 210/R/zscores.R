function (include.answer, seed) {

    set.seed(seed)

    rnorm(14, 0, 1) %>>% round(2) -> z1
    rnorm(14, 0, 1) %>>% round(2) -> z2

    ## Print question
    cat("Calculate the area between the following z scores: ")
    paste(z1, z2, sep=" and ") %>>%
        paste(collapse=", ") %>>%
        cat()

    ## Print z score table
    c(z1, z2) %>>%
        as.data.frame() %>>%
        abs() %>>%
        rename(z = 1) %>>%
        mutate(above = 1 - pnorm(z)) %>>%
        mutate(mean = 0.5 - above) %>>%
        arrange(z) %>>%
        rename("$z$"=z,
               "Area between mean and $z$"=mean,
               "Area above $z$"=above) %>>%
        xtable(digits=c(0,2,4,4)) %>>%
        print.xtable(floating=TRUE,
                     table.placement="!h",
                     sanitize.text.function=function(x){x},
                     booktabs=TRUE,
                     include.rownames=FALSE)

    if (include.answer) {

        ## Print area between each z1 and z2
        cbind(z1, z2) %>>%
            as.data.frame() %>>%
            rowwise() %>>%
            mutate(min = min(z1, z2)) %>>%
            mutate(max = max(z1, z2)) %>>%
            mutate(lesser.above = 1 - pnorm(min, lower.tail=TRUE)) %>>%
            mutate(greater.above = 1 - pnorm(max, lower.tail=TRUE)) %>>%
            mutate(between = lesser.above - greater.above) %>>%
            select(z1, z2, between) %>>%
            rename("$z_1$"=z1,
                   "$z_2$"=z2,
                   "Area between $z_1$ and $z_2$"=between) %>>%
            xtable(digits=c(0,2,2,2)) %>>%
            print.xtable(floating=TRUE,
                        table.placement="!h",
                        booktabs=TRUE,
                        sanitize.text.function=function(x){x},
                        include.rownames=FALSE)

    }    
}
