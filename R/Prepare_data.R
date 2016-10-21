#' Function to change names
.change_names <- function(x) {
  numeric_x <- as.numeric(x)
  test_num <- is.na(numeric_x)
  out_names <- c(x[test_num],paste0("Bill_",x[!test_num]))
  return(out_names)
}


#' Load the CSV files for the votes and members data
#' @import tidyverse
.load_data <- function() {

  # Load in new data and also old data on ARP (as new ARP data is not complete with bills + articles)
  arp_votes <- data.table::fread(system.file('extdata',"ARP_votes_all.csv",package='bawsala'),
                                 header=TRUE) %>% as_tibble
  names(arp_votes) <- .change_names(names(arp_votes))

  anc_votes <- data.table::fread(system.file('extdata','ANC_votes.csv',package='bawsala'),
                                 sep=',',header=TRUE,strip.white=TRUE) %>% as_tibble
  names(anc_votes) <- .change_names(names(anc_votes))

  # Load ARP bill names and labels, change bill names
  arp_vote_names <- data.table::fread(system.file('extdata','ARP_votes_names.csv',package='bawsala')) %>% as_tibble

  # Load ANC bill names and labels
  anc_vote_names <- data.table::fread(system.file('extdata','ANC_votes_labels.csv',package='bawsala'),
  col.names = c('bill.names','bill.id')) %>% as_tibble

  # Load member demographics for ANC and ARP
  arp_members <- data.table::fread(system.file('extdata',"members_ARP.csv",package='bawsala'),
                                   col.names=c('id','legis_names','sex','dob','pob','country','job','jobcat',
                                                          'elec_list','parliament_bloc')) %>% as_tibble

  anc_members <- data.table::fread(system.file('extdata',"members_ANC.csv",package='bawsala'),
                                   col.names=c('id','legis_names','sex','dob','pob','country','job',
                                                          'elec_list','parliament_bloc')) %>% as_tibble

  # Attempt a match on ARP/ANC members (fuzzy join)

  match_keys <- sapply(anc_members$legis_names,function(x) stringdist::amatch(x=x,table=arp_members$legis_names,maxDist=2))
  to_update <- arp_members$legis_names[match_keys]
  # use ARP spelling for any overllapping legislators

  anc_members <- anc_members %>% mutate(legis_names=ifelse(is.na(match_keys),legis_names,to_update))

  # Drop empty columns in the vote data

  anc_votes <- anc_votes %>% select_if(function(x) {
      count_nas <- sum(is.na(x))
      if(count_nas==length(x)) {
        FALSE
      } else {
        TRUE
      }
  })

  arp_votes <- arp_votes %>% select_if(function(x) {
    count_nas <- sum(is.na(x))
    if(count_nas==length(x)) {
      FALSE
    } else {
      TRUE
    }
  })

  anc_votes$type <- 'ANC'
  arp_votes$type <- 'ARP'
  anc_members$type <- 'ANC'
  arp_members$type <- 'ARP'

  return(list(arp_votes=arp_votes,
              arp_vote_names=arp_vote_names,
              anc_votes=anc_votes,
              anc_vote_names=anc_vote_names,
              arp_members=arp_members,
              anc_members=anc_members))

}





#' Clean Bawsalah data for analysis
#' @param keep_legis Number of legislator votes required to keep them in the dataset
#' @param refleg Character string name of reference legislator
#' @param use_subset TRUE/FALSE Use only a subset of the dataset?
#' @param if use_subset==TRUE, a vector of party bloc names with which to subset the data
#' @param use_both TRUE/FALSE Should both the ANC and ARP datasets be combined?
#' @param legis Which legislator to use if use_both==FALSE. Options are 1 for ARP and 2 for ANC
#' @param use_vb TRUE/FALSE Whether to use variational bayesian inference from the Rstan package. It is quicker but less accurate.
#' @param use_nas TRUE/FALSE For ordinal data, whether absences should be coded as a separate category (TRUE) or coded as NA (FALSE)
#' @param to_run Which of the datasets to use in the analysis. Put 1 for binary yes/no, 2 for binary yes/no v. abstain, 3 for ordinal
#' @param sample_it Whether to use a sample of the dataset for analysis. Useful for testing models.
#' @import dplyr tibble ggplot2 magrittr
#' @export
clean_data <- function(keep_legis=1,use_subset=FALSE,subset_party=c("Bloc Al Horra","Mouvement Nidaa Tounes"),
                      use_both=FALSE,refleg="ARP_Bochra Belhaj Hamida",
                      legis=1,use_vb=FALSE,use_nas=FALSE,to_run=3,sample_it=FALSE) {

  # Need to read-in all the data

  fresh_data <- .load_data()

  vote_data <- list(arp_votes=fresh_data$arp_votes,
                    anc_votes=fresh_data$anc_votes)
  member_data <- list(arp_votes=fresh_data$arp_members,
                      anc_votes=fresh_data$anc_members)

  if(to_run==1) {
    #Binary yes/no
    cleaned <- vote_data %>% lapply(function(x) {
      x <- x %>%  mutate_at(vars(starts_with("Bill")),funs(factor(.,levels = c("contre","pour"),exclude = "abstenu") %>%
                                                             as.integer))
      x <- x %>%  mutate_at(vars(starts_with("Bill")),funs(.-1))
      return(x)
    })

  } else if(to_run==2) {
    #Binary yes/no v. abstain
    cleaned <- vote_data %>% lapply(function(x) {
      x <- x %>%  mutate_at(vars(starts_with("Bill")),funs(factor(.,levels = c("contre","pour",'abstenu')) %>%
                                                             as.integer))
      x <- x %>%  mutate_at(vars(starts_with("Bill")),funs(recode(.,`2`=1,`3`=2)))
      x <- x %>%  mutate_at(vars(starts_with("Bill")),funs(`-`(.,1)))
      return(x)
    })
  } else if(to_run==3) {
    # Ordinal
    cleaned <- vote_data %>% lapply(function(x) {
      x <- x %>%  mutate_at(vars(starts_with("Bill")),funs(factor(.,levels = c("contre",'abstenu',NA,'pour')) %>%
                                                             as.integer))
      if(use_nas==TRUE) {
        x <- x %>%  mutate_at(vars(starts_with("Bill")),funs(recode(.,`3`=4,`NA`=3)))
      }
      return(x)
    })
  }


  if(use_subset==TRUE) {
  # Subset the datasets by party if one was given
    to_subset <- lapply(member_data,function(x){
     x <- x %>% filter(parliament_bloc %in% subset_party)  %>% select(legis_names)
    })
    to_subset %<>% bind_rows(to_subset) %>% distinct

    cleaned <- lapply(cleaned,function(x) {
      x %<>% filter(legis.names %in% to_subset$legis_names)
    })
  }

  if(sample_it==TRUE) {
    cleaned <- lapply(cleaned,function(x) {
      x <-    x %>% sample_n(15) %>% select_(~sample(ncol(x),size=150))
      x
    })


  }

  # Should be at least two types of votes per bill

  cleaned <- cleaned %>% lapply(function(y) {
    orig <- y %>% select(-matches("Bill"))
    y <- y %>% select(matches("Bill"))
    y <- y %>%  select_if(function(x) {
      if(length(table(x))<2) {
        FALSE
      } else {
        TRUE
      }
    })
    orig <- bind_cols(orig,y)
  })

    # Reorder based on reference legislator

  cleaned <- lapply(cleaned,function(x){
      y <-  x %>%  filter(legis.names==refleg)
      z <- x %>% filter(legis.names!=refleg)
      x <-  bind_rows(z,y)
  })


  return(cleaned)
}


#' Function to take a data set, a legislator name, a majority party, and find bills to fix positions
#' @param legislator Character string of the name of the legislator to use as a reference (ideal point fixed at 0)
#' @param party Character string of the majority party in the legislature (will be used to determine reference legislator's ideal points on bills)
#' @param party_data Data frame with all of the party and demographic information for legislators in both legislatures
#' @param vote_data Data frame with all of the votes for the particular legislature of analysis
#' @export
#' @import tidyr
fix_bills <- function(legislator=NULL,party=NULL,vote_data=NULL,legislature=NULL) {

  # Pull ref legislator votes and majority party votes

  leg_votes <- vote_data[[legislature]] %>% filter(legis.names==legislator) %>% distinct %>% gather(Bill,amount,-bloc,-id,-legis.names) %>%
    mutate(x=as.numeric(amount))

  party_votes <- vote_data[[legislature]] %>% filter(bloc==party)

    # Need party votes and also ratios of within-party votes
  party_ratio <- vote_data[[legislature]] %>% select(matches("Bill")) %>% lapply(function(x) {
    tables <- prop.table(table(x))
    # Need to check and see if there is only one vote for the party. If there was, sort will screw up the data.frame
    if(length(tables)==1) {
      tables <- tables %>% as_tibble
    } else {
      tables <- tables %>% sort(decreasing=TRUE) %>% as_tibble
    }

    return(tables)
  }) %>% bind_rows(.id="Bill") %>% group_by(Bill) %>% filter(n==max(n)) %>% distinct(.keep_all=TRUE) %>% ungroup %>%
    gather(variable,amount,-Bill)  %>% filter(variable %in% c("n","x"))  %>% mutate(amount=as.numeric(amount)) %>%
    spread(variable,amount)

  # Use the ratios of party votes for each piece of legislation to determine in which votes the reference
  # legislator did not vote with the majority party

  leg_resist <- full_join(leg_votes,party_ratio,by='Bill',suffix=c("_leg","_party")) %>% arrange(Bill) %>%
    mutate(agree=(x_leg==x_party)) %>% group_by(agree) %>% arrange(desc(n)) %>% ungroup

  abstain_gov <- leg_resist %>% filter(x_leg==2,x_party==max(x_party,na.rm=TRUE)) %>% filter(n==max(n)) %>%
    select(Bill) %>% as.character
  abstain_opp <- leg_resist %>% filter(x_leg==2,x_party==min(x_party,na.rm=TRUE)) %>% filter(n==max(n)) %>%
    select(Bill) %>% as.character

  yes_gov <- leg_resist %>% filter(agree==TRUE,x_leg==max(x_leg,na.rm=TRUE)) %>% filter(n==max(n)) %>% select(Bill) %>% slice(1) %>% as.character
  yes_opp <- leg_resist %>% filter(agree==FALSE,x_leg==max(x_leg,na.rm=TRUE)) %>% filter(n==max(n)) %>% select(Bill) %>% slice(1) %>% as.character
  no_gov <- leg_resist %>% filter(agree==TRUE, x_leg==min(x_leg,na.rm=TRUE)) %>% filter(n==max(n)) %>% select(Bill) %>% slice(1) %>% as.character
  no_opp <- leg_resist %>% filter(agree==FALSE, x_leg==min(x_leg,na.rm=TRUE)) %>% filter(n==max(n)) %>% select(Bill) %>% slice(1) %>% as.character

  final_constraint <- c(abstain_gov,abstain_opp,yes_gov,yes_opp,no_gov,no_opp)
  constraint_num <- c(-0.75,0.75,-0.25,0.25,1,-1)
  constraints <- tibble(final_constraint=final_constraint,constraint_num=constraint_num) %>% filter(grepl("Bill",final_constraint))
  return(constraints)
}


#' @export
prepare_matrix <- function(cleaned=NULL,legis=1,legislature=NULL,to_fix=NULL,use_both=FALSE) {

  # Move constrained bills to end

  cleaned <- lapply(cleaned, function(x) {
    cols_sel <- to_fix$final_constraint
    check_names <- names(x)
    cols_sel <- match(cols_sel,check_names)
    x <- bind_cols(select(x,-cols_sel),select(x,cols_sel))
  })

  if(use_both==TRUE) {
    cleaned %<>% rbind_row(cleaned)
    vote_matrix <- cleaned %>% as.matrix
  } else {

  vote_matrix <- cleaned[[legislature]] %>% select(matches("Bill")) %>% as.matrix
  }

  return(vote_matrix)
}

#' @import plotly
#' @export
plot_IRT <- function(cleaned=NULL,stan_obj=NULL,legislature=NULL) {
  means_fit <- rstan::summary(sample_fit)[[1]]
  legis_means <- as_tibble(means_fit[grepl("L_open\\[",row.names(means_fit)),])

  # Only need to use the specific legislature of interest for plotting
  if(!is.null(legislature)) {
    cleaned <- cleaned[[legislature]]
  }
  # combine estimates with vote data and plot. Order by posterior means
  legis_means <- bind_cols(legis_means,cleaned) %>% rename(estimate=`mean`)
  legis_means %<>% arrange(desc(estimate)) %>% mutate(lowci=abs(estimate-`2.5%`),highci=abs(estimate-`97.5%`),
                                                      legis.names=factor(legis.names,levels=legis.names))

  plot_ly(legis_means,x=~estimate,y=~legis.names,color=~factor(bloc)) %>%
    add_markers(error_x=~list(arrayminus=lowci,array=highci)) %>%
    layout(title="Latent Positions of Tunisian MPs",
           yaxis=~list(ticks="",title="",showticklabels = FALSE),
           xaxis=~list(title="Latent Positions"))

}
