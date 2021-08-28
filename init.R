
# Install packages
install.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages<-c("ggplot2", "dplyr", "tidyverse", "plotly", "ggthemes", "tidyr","shiny","shinydashboard")
install.packages(packages)

################################################################################
############                  Simple Random Walk                    ############
################################################################################
simple.random.walk <- function(n.steps,n.sim,prob.r=0.5){
  n <- n.steps
  a <- prob.r
  
  x.left = -1
  x.right = 1
  
  Sn_mat <- matrix(0,ncol=n+1,nrow=n.sim)
  for(i in 1:n.sim){
    for(j in 2:(n+1)){
      step <- sample(c(x.left,x.right),1,prob=c(1-a,a),replace=F)
      Sn_mat[i,j] <- Sn_mat[i,j-1] + step  
    }
  }
  # names
  names <- sapply(1:(n+1),function(i) paste('step',i,sep=''))
  # data frame
  result_df <- data.frame('sim'=sapply(1:n.sim, function(i) paste('sim',i,sep='')),
                          'Sn'=Sn_mat)
  return(result_df)
}