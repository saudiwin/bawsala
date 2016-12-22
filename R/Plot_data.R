#' @import plotly
#' @export
plot_IRT <- function(cleaned=NULL,stan_obj=NULL,legislature=NULL,plot_param=NULL,true_params='none',
                     ggplot=FALSE) {
  means_fit <- rstan::summary(stan_obj)[[1]]
  legis_means <- as_tibble(means_fit[grepl(paste0(plot_param,"\\["),row.names(means_fit)),])

  # Only need to use the specific legislature of interest for plotting
  if(!is.null(legislature)) {
    cleaned <- cleaned[[legislature]]
  }
  # combine estimates with vote data and plot. Order by posterior means
  legis_means <- bind_cols(legis_means,cleaned) %>% rename(estimate=`mean`)

  legis_means %<>% arrange(desc(estimate)) %>% mutate(lowci=abs(estimate-`2.5%`),highci=abs(estimate-`97.5%`),
                                                      legis.names=factor(legis.names,levels=legis.names),
                                                      true_params=true_params)
  if(ggplot==FALSE) {
    outplot <- plot_ly(legis_means,x=~estimate,y=~legis.names,color=~factor(bloc)) %>%
      add_markers(error_x=~list(arrayminus=lowci,array=highci)) %>%
      layout(title="Latent Positions of Tunisian MPs",
             yaxis=~list(ticks="",title="",showticklabels = FALSE),
             xaxis=~list(title="Latent Positions"))

    if(true_params!='none') {

      outplot %<>% add_markers(x=~true_params,y=~legis.names,color="red",symbols='x')
    }
  } else {
    outplot <- ggplot(legis_means,aes(x=estimate,y=reorder(legis.names,estimate),xmin=`2.5%`,xmax=`97.5%`,color=bloc)) +
      theme_minimal() +
      geom_point() + geom_errorbarh()

    if(true_params!='none') {

      outplot %<>% geom_point(aes(x=true_params,y=reorder(legis.names,estimate)),color="red")
    }
  }
  return(outplot)
}


#' Function to return item characteristic curves for ordinal response data (partial credit model)
#' @export
icc_ordinal <- function(steps=NULL,summary_data=NULL,bill_labels=NULL,param=NULL) {



}

#' Function to calculate posterior predictive values of Y
#' Currently only works for split absence with discrimination parameters.
#' @export
posterior_predict <- function(summary_data=NULL,y=NULL,ll=NULL,bb=NULL,num_iters=100) {

  summary <- rstan::extract(summary_data,permuted=TRUE,inc_warmup=FALSE)
  all_iter <- nrow(summary[[1]])
  to_use <- sample(all_iter,num_iters)
  nsteps <- ncol(summary$steps_votes) + 1
  summary <- lapply(summary,function(x) {
    if(length(dim(x))>1) {
      x <- x[to_use,]
    return(t(x))
  }
  })

  Y_rep <- matrix(nrow=length(y),ncol=100)

  for(i in 1:100) {
    for(n in 1:length(y)) {
      # Only calculate ordinal model if legislator is present
      pi1 <- plogis(summary$sigma_abs_open[bb[n],i] * summary$L_open[ll[n],i] - summary$B_abs[bb[n],i])
      absent <- rbinom(n=1,size=1,prob=pi1)
      if(absent==1) {
        Y_rep[n,i] <- 4
      } else {
        # Construct cutpoints and probabilities, then sample one element from probabilities of each outcome occuring
        all_probs <- 1 - plogis((summary$sigma_adj[bb[n],i] * summary$L_open[ll[n],i] - summary$B_yes[bb[n],i]) - summary$steps_votes[1,i])
        all_probs <- c(all_probs,
                       plogis((summary$sigma_adj[bb[n],i] * summary$L_open[ll[n],i] - summary$B_yes[bb[n],i]) - summary$steps_votes[1,i]) -
                         plogis((summary$sigma_adj[bb[n],i] * summary$L_open[ll[n],i] - summary$B_yes[bb[n],i]) - summary$steps_votes[2,i]),
                       plogis((summary$sigma_adj[bb[n],i] * summary$L_open[ll[n],i] - summary$B_yes[bb[n],i]) - summary$steps_votes[2,i]))
        Y_rep[n,i] = sample(x = 1:nsteps,size=1,prob = all_probs)
      }

    }
    print(paste0('Finished iteration ',i))
  }
 return(Y_rep)
}
