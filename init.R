
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

################################################################################
##################### Caminata simple en dos dimensiones  ######################
################################################################################
randomWalk2d_plot <- function(base, n.steps){
  
  df <- base
  df_2d <- df  %>%
    gather(key='t',value='valor',-sim) %>%
    filter(sim == 'sim1' | sim=='sim2') %>%
    spread(sim,valor)  %>%
    mutate(t = as.numeric(substring(t,4,10))) %>%
    arrange(t) %>%
    filter(t <= n.steps)
  b2 <- ggplot(df_2d,aes(x=sim1,y=sim2))+
    geom_point(color="blue") +
    geom_point(df_2d%>%filter(t == 1),mapping=aes(x=sim1,y=sim2),color="green") +
    geom_point(df_2d%>%filter(t == max(t)),mapping=aes(x=sim1,y=sim2),color="red") +
    geom_path() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank())
  return(b2)
}

################################################################################
################## Caminata simple en tres dimensiones  ########################
################################################################################
randomWalk3d_plot <- function(base, n.steps){
  
  df <- base
  df_2d <- df  %>%
    gather(key='t',value='valor',-sim) %>%
    filter(sim == 'sim1' | sim=='sim2' | sim == 'sim3') %>%
    spread(sim,valor)  %>%
    mutate(t = as.numeric(substring(t,4,10))) %>%
    arrange(t) %>%
    filter(t <= n.steps)
  b3 <- plot_ly(x=df_2d$sim1, y=df_2d$sim2, z=df_2d$sim3, type="scatter3d", mode="lines")
  
  return(b3)
}
