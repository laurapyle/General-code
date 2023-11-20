dat <- NULL

################################
# LESS THAN 4 PROGRAM ELEMENTS #
################################


  #######################
  #2) Input parameters  #
  #######################

  set.seed(3654) #change from development/ testing to production

  n_groups<-3 #number of study groups
  block_size<-3  #must be a multiple of the sum of ratio of group allocation, i.e. 1:1 needs multiple of 2, 
                #also needs to be multiple of # stratification groups, i.e. 2 sex levels x 2 age groups
  n_tot<-6  #must be a multiple of the block size, inflate needed sample size to allow for additional randomizations

  #############################
  #3) Generate randomization  #
  #############################
  x<-NULL #randomization variable
  xy <- NULL

  z<-seq(1,n_tot,block_size) #set break points for each block 

  #group assignment for each block
  for(i in z){
    x[i:(i+(block_size-1))]<-c(sample(c(rep(1:n_groups,each=block_size/n_groups))))
    x
  }
  
  # combine groups 2 and 3
  x[x==3] <- 2
  
  # add site names
  y <- c("Children's Healthcare of Atlanta","Arkansas Children's Hospital","Alfred I DuPont Hospital for Children",
         "University of Hawaii","St. Louis Children's Hospital", "Levine Children's Hospital at Atrium Health")
  
  xy <- cbind(x, y)
  strata <- rep(">= 4 program elements",6)
  xy <- cbind(xy, strata)
  
  ##########################################
  #4) Create randomization allocation table#
  ##########################################

  #add randomization to dataset created earlier
  dat <- xy

  
################################
# LESS THAN 4 PROGRAM ELEMENTS #
################################

  #######################
  #2) Input parameters  #
  #######################
  
  set.seed(3654) #change from development/ testing to production
  
  n_groups<-3 #number of study groups
  block_size<-3  #must be a multiple of the sum of ratio of group allocation, i.e. 1:1 needs multiple of 2, 
  #also needs to be multiple of # stratification groups, i.e. 2 sex levels x 2 age groups
  n_tot<-6  #must be a multiple of the block size, inflate needed sample size to allow for additional randomizations
  
  #############################
  #3) Generate randomization  #
  #############################
  x<-NULL #randomization variable
  xy <- NULL
  
  z<-seq(1,n_tot,block_size) #set break points for each block 
  
  #group assignment for each block
  for(i in z){
    x[i:(i+(block_size-1))]<-c(sample(c(rep(1:n_groups,each=block_size/n_groups))))
    x
  }

  # combine groups 2 and 3
  x[x==3] <- 2
  
  # add site names
  y <- c("OHSU","University of Texas","University of Nebraska Medical Center","University of Wisconsin","Tampa General Hospital Children's Medical",
         "University of Vermont")
  
  xy <- cbind(x, y)
  strata <- rep("<= 4 program elements",6)
  xy <- cbind(xy, strata)
  
  dat<-rbind(dat, xy)
  
  dat<-data.frame(dat)
  names(dat)=c("Intervention", "Site Name", "Strata")
  
#check
table(dat$Intervention,dat$Strata)

#export
write.csv(dat,'/Volumes/pylell/Rao/site_randomization.csv',row.names=FALSE)
