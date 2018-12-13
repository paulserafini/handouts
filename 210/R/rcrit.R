function (n, alpha) {
  df <- n - 2
  tcrit <- qt(alpha/2, df, lower.tail = F)
  rcrit <- sqrt((tcrit^2) / ((tcrit^2) + df))
  return(rcrit)
}
