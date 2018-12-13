function(include.answer, seed) {

    set.seed(seed)

    x <- sample(1:10, 5)
    y <- sample(1:10, 5)

    if (include.answer == TRUE) {
        output <- paste0("
                          \\begin{gather*}
                          \\Sigma(X_i) = ", sum(x), " \\\\
                          [\\Sigma(X_i)]^2 = ", sum(x)^2, " \\\\
                          \\Sigma(X_i - Y_i) = ", sum(x - y), " \\\\
                          \\Sigma(X_iY_i) = ", sum(x * y), " \\\\
                          \\Sigma(X_i^2) = ", sum(x^2), " \\\\
                          [\\Sigma(X_i - Y_i)]^2 = ", sum(x - y)^2, " \\\\
                          [\\Sigma(X_i)][\\Sigma(Y_i)] = ", sum(x) * sum(y), " \\\\
                          \\end{gather*}")
    } else {
        output <- paste0("
                          \\begin{gather*}
                          \\Sigma(X_i) =  \\\\
                          [\\Sigma(X_i)]^2 =  \\\\
                          \\Sigma(X_i - Y_i) =  \\\\
                          \\Sigma(X_iY_i) =  \\\\
                          \\Sigma(X_i^2) =  \\\\
                          [\\Sigma(X_i - Y_i)]^2 =  \\\\
                          [\\Sigma(X_i)][\\Sigma(Y_i)] =  \\\\
                          \\end{gather*}")
    }

    cat("/X/ = ", paste0(x, sep=" "), "\\\\", "/Y/ = ", paste0(y, sep=" "), output, sep="\n")

}
