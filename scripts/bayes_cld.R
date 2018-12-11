get_p_matrix <- function(df, only_low = TRUE) {
  # pre-define matrix
  out <- matrix(-1, nrow = ncol(df), ncol = ncol(df), dimnames = list(colnames(df), colnames(df)))
  for (i in seq_len(ncol(df))) {
    for (j in seq_len(ncol(df))) {
      out[i, j] <- mean(df[,i] < df[,j]) 
    }
  }
  if (only_low) out[out > .5] <- 1- out[out > .5]
  out
}
cld_pmatrix <- function(model, pars, level = 0.05) {
  p_matrix <- get_p_matrix(model)
  lp_matrix <- (p_matrix < (level/2) | p_matrix > (1-(level/2)))
  cld <- multcompView::multcompLetters(lp_matrix)$Letters
  cld
}