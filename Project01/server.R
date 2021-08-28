
library('shiny')
library('shinydashboard')

server <- function(input, output){
  # Dataset
  df1 <- reactive({
    df <- simple.random.walk(input$nsteps,input$nrep,prob.r=input$a)
    df
  }) 
  
  output$RandomWalk_trayectories <- renderPlot({
    df_rw <- df1()  %>%
      gather(key='t',value='valor',-sim) %>%
      mutate(t = as.numeric(substring(t,4,10))) %>%
      arrange(sim)
    moments_rw <- data.frame('t'=c(1:input$nsteps),'a'=input$a) %>%
      mutate('mean'=t*(a-(1-a)),
             'sd_sup'=mean + 2*sqrt(4*t*a*(1-a)),
             'sd_inf'=mean - 2*sqrt(4*t*a*(1-a))) %>%
      filter(t <= input$nsteps)
    
    p1 <- ggplot(df_rw,aes(x=t,y=valor,color=sim)) +
      geom_line() +
      geom_line(moments_rw, mapping=aes(x=t,y=mean),col='red',size=0.7) +
      geom_line(moments_rw, mapping=aes(x=t,y=sd_sup),col='blue',size=0.7,linetype = "dashed") +
      geom_line(moments_rw, mapping=aes(x=t,y=sd_inf),col='blue',size=0.7,linetype = "dashed") +
      scale_colour_grey(start = 0.2,end = 0.6) +
      theme(legend.position="none") +
      xlab("") +
      ylab("")+
      ggtitle(paste(input$nrep," Simple Random Walk trajectories",sep=''))
    p1
  })
  
  output$RandomWalk_distribution <- renderPlot({
    df_rw <- df1() %>%
      gather(key='t',value='valor',-sim) %>%
      mutate(t = as.numeric(substring(t,4,10))) %>%
      arrange(sim) %>%
      filter(t <= input$nsteps)
    t.selected <- input$nsteps
    df_dist <- df_rw %>% filter(t==t.selected)
    
    p2 <- ggplot(df_dist,aes(valor)) +
      geom_histogram(bins=15,fill='red',col="white") +
      #coord_flip() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank())
    p2  
  })
  
  output$RandomWalk_2d <- renderPlot({
    
    p3 <- randomWalk2d_plot(base = df1() ,n.steps = input$nsteps)
    p3
  } )
  
  output$RandomWalk_3d <- renderPlotly({
    
    p4 <- randomWalk3d_plot(base = df1() ,n.steps = input$nsteps)
    p4
  } )
  
}