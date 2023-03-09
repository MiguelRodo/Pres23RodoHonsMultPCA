
calc_proj_length <- function(x, y, proj_vec) {
  orig_mat <- matrix(x, y, byrow = FALSE, ncol = 2)
  proj_vec <- proj_vec / sum(proj_vec * proj_vec)
  (orig_mat %*% proj_vec) |> as.numeric()
}

calc_proj <- function(orig_mat, proj_vec) {
  proj_vec <- proj_vec / sum(proj_vec * proj_vec)
  c_vec <- calc_proj_length(orig_mat[, 1], orig_mat[, 2], proj_vec)
  proj_mat <- matrix(rep(0, 2 * nrow(orig_mat)), ncol = 2)
  for (i in seq_len(nrow(proj_mat))) {
    proj_mat[i, ] <- c_vec[i] * proj_vec
  }
  proj_mat
}
