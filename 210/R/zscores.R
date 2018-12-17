function (include.answer, seed) {

    ## Generate example
    set.seed(seed)
    z1 <- runif(14, 0.05, 0.95)
    z2 <- runif(14, 0.05, 0.95)
    massRound(z1, z2)

    ## Print question
    z.list <- paste(z1, z2, sep=" and ")
    z.list <- paste(z.list, collapse=", ")
    cat("Calculate the area between the following z scores: ")
    cat(z.list)

    ## Create z score table
    z <- unique(z1, z2)
    z <- abs(z)
    z <- sort(z)
    table <- cbind(z)
    table <- transform(table, above = 1 - pnorm(z))
    table <- transform(table, mean = 0.5 -above)
    colnames(table) <- c("$z$",
                         "Area between mean and $z$",
                         "Area above $z$")
    table <- xtable(table, digits=c(0,2,4,4))
    print.xtable(table,
                 floating=TRUE,
                 table.placement="!h",
                 sanitize.text.function=function(x){x},
                 booktabs=TRUE,
                 include.rownames=FALSE)

    if (include.answer) {

        ## Print area between each z1 and z2
        table <- cbind(z1, z2)
        min <- apply(table, 1, min)
        max <- apply(table, 1, max)
        area.above.min <- 1 - pnorm(min, lower.tail=TRUE)
        area.above.max <- 1 - pnorm(max, lower.tail=TRUE)
        area.between <- area.above.min - area.above.max
        table <- cbind(table, area.between)
        colnames(table) <- c("$z_1$", "$z_2$", "Area between $z_1$ and $z_2$")

        table <- xtable(table, digits=c(0,2,2,2))

        print.xtable(table,
                     floating=TRUE,
                     table.placement="!h",
                     booktabs=TRUE,
                     sanitize.text.function=function(x){x},
                     include.rownames=FALSE)


    }    
}
