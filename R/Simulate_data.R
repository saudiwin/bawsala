# Functions to simulate ordinal IRT data

#' A function taken from Daniel Furr's edstan package for the Rating Scale Model
#' @export
simulate_cleaned <- function(num_legis=50,num_bills=100,num_cuts=3,legislature=NULL,
                             parties=3) {
  # Legislator ideal points, correlated within parties

  party_blocs <- round(runif(n = num_legis,min = 1,max=parties))
  party_ideals <- seq(from=-2,to=2,length.out=parties)
  theta <- 1:num_legis
  for(l in 1:num_legis) {
    theta[l] <- rnorm(1, party_ideals[party_blocs[l]], 0.2)
  }

  alpha <- runif(n= num_bills,min=-5,max=5)
  beta <- seq(from = -1, to = 1, length.out = num_bills)
  kappa <- seq(from = -1, to = 1, length.out = num_cuts - 1)
  N <- num_legis*num_bills
  y <- num_legis*num_bills
  ll <- rep(1:num_legis,times=num_bills)
  bb <- rep(1:num_bills,each=num_legis)
  for(n in 1:N) {
    unsummed <- c(0, alpha[bb[n]] * (theta[ll[n]] - beta[bb[n]] - kappa))
    numerators <- exp(cumsum(unsummed))
    denominator <- sum(numerators)
    response_probs <- numerators/denominator
    y[n] <- sample(1:length(response_probs) - 1, size = 1, prob = response_probs)
  }
  y <- y + 1
  total_matrix <- matrix(y,nrow=num_legis,ncol=num_bills)
  colnames(total_matrix) <- paste0("Bill_",1:num_bills)
  out_data <- as_data_frame(total_matrix) %>% mutate(legis.names=as.character(1:n()),
                                                     bloc=as.character(party_blocs),
                                                     id=as.character(1:n()),
                                                     type=legislature)
  return_list <- list(out_data)
  names(return_list) <- legislature
  return(list(cleaned=return_list,true_legis=theta,true_bill=beta,true_sigma=alpha,true_cut=kappa))
}
