function(include.answer, seed) {

    set.seed(seed)

    ## Create scenario
    width <- round(runif(1, 1, 50), 0)
    lowest <- round(runif(1, 1, 100), 0)

    ## Create question
    question <- paste0("The lowest value in the data set is ", lowest,
                       ", and the desired interval width is ", width,
                       ". Find the lower limit, midpoint, and upper limit of the first five intervals.")

    if (include.answer == TRUE) {

        cat(question)

        ## Find LL1 and LL5
        numbers <- 0:500
        multiples <- numbers[numbers%%width == 0]
        multiples <- multiples[multiples <= lowest]
        LL1 <- max(multiples)
        LL5 <- LL1 + width * 4

        ## Derive LL, MP, and UL of first five intervals
        seq(from = LL1, to = LL5, by = width) %>>%
            as.data.frame() %>>%
            rename(LL = 1) %>>%
            mutate(UL = LL + width - 1) %>>%
            rowwise() %>% 
            mutate(MP=mean(c(LL,UL))) %>>%
            select(LL, MP, UL) %>>%
            xtable(digits=c(0,0,1,0)) %>>%
            print.xtable(floating=TRUE,
                         table.placement="!h",
                         booktabs=TRUE,
                         include.rownames=FALSE)
        
    } else {

        cat(question, "\n")

    }

}
