#' Linear regression with QR
#'
#' @param formula A forumula describin the model
#' @param data A data.frame containing the data
#'
#' @return Returns a linear regression model
#' @export
#'
#' @examples
#' linreg(Sepal.Length ~ Species, data = iris)
#'
linreg <- function(formula, data){


  X <- stats::model.matrix(formula, data = data)
  y <- as.matrix(data[,all.vars(formula)[1], drop = FALSE])

  qr_object <- base::qr(X)

  Q <- base::qr.Q(qr_object)
  R <- base::qr.R(qr_object)


  beta_vec <- solve(R)%*%t(Q)%*%y


  fitted_values <- X%*%beta_vec


  df <- nrow(X) - ncol(X)

  residuals <- y - fitted_values

  residual_variance <- as.numeric((t(residuals) %*% residuals)/df)

  error_betas <- sqrt(diag(residual_variance*(solve(R) %*% t(solve(R)))))


  beta_names <- rownames(beta_vec)
  beta_vec <- as.vector(beta_vec)
  names(beta_vec) <- beta_names

  reg_list <- list(beta = beta_vec,
                   fits = fitted_values,
                   df = df,
                   residuals = residuals,
                   res_var = residual_variance,
                   error_betas = error_betas,
                   formula = deparse(substitute(formula)),
                   data = deparse(substitute(data)))


  class(reg_list) <- "linreg"
  return(reg_list)
}
