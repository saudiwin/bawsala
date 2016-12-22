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
                                 header=TRUE,encoding='Latin-1') %>% as_tibble
  names(arp_votes) <- .change_names(names(arp_votes))

  anc_votes <- data.table::fread(system.file('extdata','ANC_votes.csv',package='bawsala'),
                                 sep=',',header=TRUE,strip.white=TRUE,encoding='Latin-1') %>% as_tibble
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
#' @param sample_amt Integer number of legislators to sample from dataset
#' @import dplyr tibble ggplot2 magrittr
#' @export
clean_data <- function(keep_legis=1,use_subset=FALSE,subset_party=c("Bloc Al Horra","Mouvement Nidaa Tounes"),
                      use_both=FALSE,refleg="ARP_Bochra Belhaj Hamida",
                      legis=1,use_vb=FALSE,use_nas=FALSE,to_run=3,sample_it=FALSE,
                      sample_amt=50) {

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
        x %<>% mutate_at(vars(starts_with("Bill")),
                        funs(recode(., `3` = 4L,.missing=3L)))
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
      if(nrow(x)>sample_amt) {
      x <-    x %>% sample_n(sample_amt)
      }
      all_bills <-  grep('Bill',names(x),value=TRUE)
      bills_sample <- sample(all_bills,100)
      bills_sample <- match(bills_sample,names(x))
      x %<>% select(id,legis.names,bloc,type,bills_sample)
      return(x)
    })


  }

  # Should be at least two types of votes per bill for ordinal & binary, three types for ordinal with more than
  # 3 categories
  if(use_nas==FALSE) {
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
  } else if(use_nas==TRUE) {
    cleaned <- cleaned %>% lapply(function(y) {
      orig <- y %>% select(-matches("Bill"))
      y <- y %>% select(matches("Bill"))
      y <- y %>%  select_if(function(x) {
        if(length(table(x))<3) {
          FALSE
        } else {
          TRUE
        }
      })
      orig <- bind_cols(orig,y)
    })
  }

    # Order dataset by legislator names

  cleaned <- lapply(cleaned,function(x){
      # y <-  x %>%  filter(legis.names==refleg)
      # z <- x %>% filter(legis.names!=refleg)
      x <- arrange(x,legis.names)
  })


  return(cleaned)
}

#' Function to take a roll call data set and the names of opposition and governing parties.
#' The opposition party should be as extreme in opposition as possible.
#' This function helps to achieve identification by constraining discrimination parameters.
#' For bills on which the opposition votes in majority, discrimination is constrained to be
#' positive.
#' For bills on which the government votes in majority, discrimination is constrained to be
#' negative.
#' For other bills, discrimination is free to float.
#' @param opp Character, name of opposition party to filter data
#' @param gov Character, name of governing party to filter data
#' @param vote_data Voting data in list form, with each element of list equal to a legislature
#' @param legislature The specific legislature in the list to choose
#' @export
fix_bills_discrim <- function(opp=NULL,gov=NULL,vote_data=NULL,legislature=NULL,to_run=NULL,use_nas=NULL) {

  # Create long rollcall vote datasets filtered by party

  gov_votes <- vote_data[[legislature]] %>% filter(bloc==gov) %>% distinct

  ngov <- nrow(gov_votes)

  gov_votes %<>% gather(Bill,amount,matches('Bill'))
  if(to_run==3 & use_nas==FALSE) {
  gov_votes <- gov_votes %>% group_by(Bill) %>% summarize(yes=mean(amount==3,na.rm=TRUE),
                                                          no=mean(amount==1,na.rm=TRUE),
                                                          abstain=mean(amount==2,na.rm=TRUE),
                                                          quorum=sum(amount %in% c(1,2,3),na.rm=TRUE)/ngov) %>%
    filter(yes>.8, quorum>.6)
  } else if(to_run==3 & use_nas==TRUE) {
    gov_votes <- gov_votes %>% group_by(Bill) %>% summarize(yes=mean(amount==4,na.rm=TRUE),
                                                            no=mean(amount==1,na.rm=TRUE),
                                                            abstain=mean(amount==2,na.rm=TRUE),
                                                            quorum=sum(amount %in% c(1,2,4),na.rm=TRUE)/ngov) %>%
      filter(yes>.8, quorum>.6)
    }else {
    gov_votes <- gov_votes %>% group_by(Bill) %>% summarize(yes=mean(amount==1,na.rm=TRUE),
                                                            no=mean(amount==0,na.rm=TRUE),
                                                            quorum=sum(amount %in% c(1,0),na.rm=TRUE)/ngov) %>%
      filter(yes>.8, quorum>.6)
}
  opp_votes <- vote_data[[legislature]] %>% filter(bloc==opp) %>% distinct

  nopp <- nrow(opp_votes)

  opp_votes %<>% gather(Bill,amount,matches("Bill"))
  if(to_run==3 & use_nas==FALSE) {
  opp_votes <- opp_votes %>% group_by(Bill) %>% summarize(yes=mean(amount==3,na.rm=TRUE),
                                                          no=mean(amount==1,na.rm=TRUE),
                                                          abstain=mean(amount==2,na.rm=TRUE),
                                                          quorum=sum(amount %in% c(1,2,3),na.rm=TRUE)/nopp) %>%
    filter(yes>.8,quorum>.6)
  } else if(to_run==3 & use_nas==TRUE) {
    opp_votes <- opp_votes %>% group_by(Bill) %>% summarize(yes=mean(amount==4,na.rm=TRUE),
                                                            no=mean(amount==1,na.rm=TRUE),
                                                            abstain=mean(amount==2,na.rm=TRUE),
                                                            quorum=sum(amount %in% c(1,2,4),na.rm=TRUE)/nopp) %>%
    filter(yes>.8,quorum>.6)
  } else {
    opp_votes <- opp_votes %>% group_by(Bill) %>% summarize(yes=mean(amount==1,na.rm=TRUE),
                                                            no=mean(amount==0,na.rm=TRUE),
                                                            quorum=sum(amount %in% c(0,1),na.rm=TRUE)/nopp) %>%
      filter(yes>.8,quorum>.6)
}
  #remove any bills that both opp and gov voted for
  to_remove <- opp_votes$Bill[opp_votes$Bill %in% gov_votes$Bill]

  opp_votes <- opp_votes %>% filter(!(Bill %in% to_remove))

  gov_votes <- gov_votes %>% filter(!(Bill %in% to_remove))

  # Figure out bill to set at discrimination zero: these are bills that are not loading in either dimension very well

 no_bill <-  vote_data[[legislature]] %>% gather(bill_num,bill_vote,matches('Bill')) %>%
    group_by(bill_num) %>% summarize(num_miss=sum(is.na(bill_vote)),nos=mean(bill_vote==1,na.rm=TRUE)/num_miss) %>%
    arrange(-nos)

 # Figure out which bills should be loaded negative (government) on absence
 abs_votes <- vote_data[[legislature]] %>% filter(bloc==gov) %>% distinct

 abs_votes %<>% gather(Bill,amount,matches('Bill'))

  abs_gov_votes <- abs_votes %>% group_by(Bill) %>% summarize(yes=mean(amount==4,na.rm=TRUE),
                                                              no=mean(amount==1,na.rm=TRUE),
                                                              abstain=mean(amount==2,na.rm=TRUE),
                                                              quorum=sum(amount %in% c(1,2,4),na.rm=TRUE)/ngov)

  abs_votes <- vote_data[[legislature]] %>% filter(bloc==opp) %>% distinct

  abs_votes %<>% gather(Bill,amount,matches('Bill'))

  abs_opp_votes <- abs_votes %>% group_by(Bill) %>% summarize(yes=mean(amount==4,na.rm=TRUE),
                                                              no=mean(amount==1,na.rm=TRUE),
                                                              abstain=mean(amount==2,na.rm=TRUE),
                                                              quorum=sum(amount %in% c(1,2,4),na.rm=TRUE)/ngov)

  abs_gov_votes %<>% arrange(quorum)
  abs_opp_votes %<>% arrange(quorum)
  abs_gov <- abs_gov_votes$Bill[1:10]
  abs_opp <-abs_opp_votes$Bill[1:10]

  abs_votes <- abs_opp[!(abs_opp %in% abs_gov)]

  return(list(gov=gov_votes$Bill,opp=opp_votes$Bill,no_bill=no_bill$bill_num[1],abs=abs_votes))

}


#' Function to take a data set, a legislator name, a majority party, and find bills to fix positions
#' By fixing bills compared to the reference legislator. This type of identification has not performed well so
#' far.
#' @param legislator Character string of the name of the legislator to use as a reference (ideal point fixed at 0)
#' @param party Character string of the majority party in the legislature (will be used to determine reference legislator's ideal points on bills)
#' @param party_data Data frame with all of the party and demographic information for legislators in both legislatures
#' @param vote_data Data frame with all of the votes for the particular legislature of analysis
#' @export
#' @import tidyr
fix_bills_refleg <- function(legislator=NULL,party=NULL,vote_data=NULL,legislature=NULL) {

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
  names(final_constraint) <- c('abstain_gov','abstain_opp','yes_gov','yes_opp','no_gov','no_opp')
  constraint_num <- c(-0.75,0.75,-0.25,0.25,1,-1)
  constraints <- tibble(final_constraint=final_constraint,constraint_num=constraint_num,
                        bill_type=names(final_constraint)) %>% filter(grepl("Bill",final_constraint))
  return(constraints)
}



#' Accepting already cleaned data, this function will split the outcome variable into a no --> abstain --> yes (1,2,3) component
#' And a absent/party votes no --> absent/party split --> absent/party votes yes (4,5,6)
#' The intention is to enable separate modeling of these two types of responses to legislative votes
split_absences <- function(cleaned=NULL,to_fix=NULL) {

  #simple function to change the votes with new vectors
  change_votes <- function(vote=NULL,yes=NULL,no=NULL) {
    vote_new <- vote
    vote[vote==3 & yes>.6] <- 6
    vote[vote==3 & no>.6] <- 4
    vote[vote==3 & (yes<.6 & no<.6)] <- 5
    vote[vote_new==4] <- 3
    return(vote)
  }


  lapply(cleaned, function(x,refbill=NULL) {
    if(length(grep('Bill',names(x),value=TRUE))==0) {
      return(x)
    }
    bloc_bill <- gather(x,bill,vote,matches('Bill')) %>%
      group_by(bloc,bill) %>% mutate(yes=sum(vote==4)/length(bloc),no=sum(vote==1)/length(bloc))

    bloc_bill <- bloc_bill %>% mutate(vote_orig=vote,vote=change_votes(vote,yes,no)) %>%
      ungroup %>% select(legis.names,bloc,type,bill,vote) %>% spread(key=bill,value=vote) %>% arrange(legis.names)

    if(refbill[1] %in% names(bloc_bill)) {
      bloc_bill <- select(bloc_bill,-one_of(refbill),one_of(refbill))
    }

    return(bloc_bill)

  },refbill=to_fix)
}


#' @export
prepare_matrix <- function(cleaned=NULL,legis=1,legislature=NULL,to_fix=NULL,to_fix_type=NULL,
                           use_both=FALSE,
                           to_pin_bills=NULL,
                           absent_bill=NULL,
                           only_gov=TRUE,split_absences=FALSE,to_run=NULL,use_nas=NULL) {

  if(to_fix_type=='ref_bills') {
    # Move constrained bills to end
    to_fix <- to_fix %>% filter(bill_type %in% to_pin_bills) %>% arrange(bill_type)
    cleaned <- lapply(cleaned, function(x) {
      cols_sel <- to_fix$final_constraint
      check_names <- names(x)
      cols_sel <- match(cols_sel,check_names)
      x <- bind_cols(select(x,-cols_sel),select(x,cols_sel))
    })
  } else if(to_fix_type=='ref_discrim') {
    names_cleaned <- names(cleaned)
    cleaned <- lapply(cleaned, function(x) {
      check_names <- names(x)
      cols_sel_opp <- match(to_fix$opp,check_names)
      cols_sel_gov <- match(to_fix$gov,check_names)
      cols_sel_nobill <- match(to_fix$no_bill,check_names)
      cols_sel_opp <- cols_sel_opp[!is.na(cols_sel_opp)]
      cols_sel_gov <- cols_sel_gov[!is.na(cols_sel_gov)]
      cols_sel_nobill <- cols_sel_nobill[!is.na(cols_sel_nobill)]
      if(only_gov==TRUE) {
        cols_sel_opp <- NULL
        cols_sel_nobill <- NULL
      }
      if(any(length(cols_sel_gov)>0,length(cols_sel_nobill)>0,length(cols_sel_opp)>0)) {
      x <- bind_cols(select(x,-c(cols_sel_opp,cols_sel_gov)),select(x,c(cols_sel_opp,cols_sel_gov)))
      }
      return(list(data=x,opp_num=length(cols_sel_opp),gov_num=length(cols_sel_gov)))
    })
      opp_num <- cleaned[[legislature]]$opp_num
      gov_num <- cleaned[[legislature]]$gov_num
      cleaned <- lapply(cleaned,function(x) x$data)
      names(cleaned) <- names_cleaned
  }

  # Split absences into three underlying categories based on how legislators voted
  if(split_absences==TRUE) {
    if(to_run<3) {
      stop("Splitting absences is only for ordinal data. Please re-run with to_run set to 3.")
    }
    if(use_nas==FALSE) {
      stop("Splitting absences requires cleaned ordinal data with absences as a separate category. Please set use-nas to TRUE.")
    }
    cleaned <- split_absences(cleaned,to_fix=absent_bill)
  }

  if(use_both==TRUE) {
    cleaned %<>% rbind_row(cleaned)
    vote_matrix <- cleaned %>% select(matches('Bill')) %>% as.matrix
  } else {

  vote_matrix <- cleaned[[legislature]] %>% select(matches("Bill")) %>% as.matrix
  }

  return(list(votes=vote_matrix,opp_num=opp_num,gov_num=gov_num))
}


