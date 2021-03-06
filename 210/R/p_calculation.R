function (include.answer, seed) {

    ## Generate example
    set.seed(seed)
    n <- 48
    positive <- seq(from=0.01, to=0.99, by=0.01)
    positive <- sample(positive, 57)
    both <- c(positive, -positive)
    z1 <- sample(both, n)
    z2 <- sample(both, n)

    ## Print area between each z1 and z2
    table <- cbind(z1, z2)
    min <- apply(table, 1, min)
    max <- apply(table, 1, max)
    area.above.min <- 1 - pnorm(min, lower.tail=TRUE)
    area.above.max <- 1 - pnorm(max, lower.tail=TRUE)
    area.between <- area.above.min - area.above.max
    massRound(area.above.min, area.above.max, area.between)
    area.between <- paste0(area.above.min, " - ", area.above.max, " = ", area.between)

    if (!include.answer) {
        area.between[6:n] <- ""
    }

    table <- cbind(table, area.between)
    colnames(table) <- c("$z_1$", "$z_2$", "Area between $z_1$ and $z_2$")

    table <- xtable(table,
                    align=c("r","r","r","r"),
                    digits=c(0,2,2,2))

    print.xtable(table,
                 floating=TRUE,
                 table.placement="!h",
                 booktabs=TRUE,
                 sanitize.text.function=function(x){x},
                 include.rownames=FALSE)


    
    ## Create z score table
    z <- sort(positive)
    table <- cbind(z)
    table <- transform(table, above = 1 - pnorm(z))
    table <- transform(table, mean = 0.5 -above)
    colnames(table) <- c("$z$",
                         "Area above $z$",
                         "Area between mean and $z$")

    table <- xtable(table, digits=c(0,2,4,4))
    print.xtable(table,
                 floating=TRUE,
                 table.placement="!h",
                 sanitize.text.function=function(x){x},
                 booktabs=TRUE,
                 include.rownames=FALSE)


}
