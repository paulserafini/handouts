function (table, header) {

	    table <- rbind(colnames(table), table)


    cat("#+ATTR_LATEX: :booktabs t :center t :rmlines t\n")
        cat("|----------------+-------+---------+---------+----------|\n")
        for (i in 1:nrow(table)) { # Print each row on its own line with | between each element
		        if (i == 2 & header == TRUE) { # If there's a header row, don't bound it with |
				            cat("|----------------+-------+---------+---------+----------|\n")
				             }
	        cat("   | ")
		        cat(table[i,], sep=" | ")
		        cat(" | \n")
			    }
	    cat("|----------------+-------+---------+---------+----------|\n")
}
