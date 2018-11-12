#define library
library(shiny)
library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)
shinyServer(
  
  function(input,output)
  {
    ################################## Basic Tab ########################################
    dataset= reactive({
      
      switch(input$season,
             "summer_data" =  names(summer_data[,c(2,3,4,5,6)]),
             "winter_data" = names(winter_data[,c(2,3,4,5,6)])
      )
    })
    
    output$v = renderUI({
      
      selectInput("type","SELECT THE POLLUTANT",choices = dataset())
      
    })
    
    
    output$data= renderTable({
      attach(get(input$season))
      get(input$type)
    })
    
    
    #Graphs
    output$mygraph=renderPlot({
      attach(get(input$season))
      y=get(input$type)
      
      switch(input$plot,
             # "Histogram" = hist(get(input$type),freq=FALSE,xlab="Days",ylab="mole per square",prob=T,breaks = seq(0,max(get(input$type)),l=input$bins+1),
             #col=input$color),
             "Line" = switch(input$season,
                             "summer_data" = ggplot(database1, aes(x=s_date,y=y)) + geom_line(stat='identity', position='dodge')+xlab("Time in Days")+ylab("Pollutants in Ug/m3"),
                             "winter_data" = ggplot(database1, aes(x=w_date,y=y)) + geom_line(stat='identity', position='dodge')+xlab("Time in Days")+ylab("Pollutants in Ug/m3"),
             ),
             
             "Points" = switch(input$season,
                               "summer_data" = ggplot(database1, aes(x=s_date,y=y)) + geom_point()+xlab("Time in Days")+ylab("Pollutants in Ug/m3"),
                                 "winter_data" = ggplot(database1, aes(x=w_date,y=y)) + geom_point()+xlab("Time in Days")+ylab("Pollutants in Ug/m3"),
             ),
             "Both_L_&_P" = switch(input$season,
                                   "summer_data" =  plot(s_date,get(input$type),type="b",xlab="Time in Days",ylab="Pollutants in Ug/m3") ,
                                   "winter_data" = plot(w_date,get(input$type),type="b",xlab="Time in Days",ylab="Pollutants in Ug/m3" ),
             ),
             "Stairs" = switch(input$season,
                               "summer_data" =  plot(s_date,get(input$type),type="s",xlab="Time in Days",ylab="Pollutants in Ug/m3" ),
                               "winter_data" = plot(w_date,get(input$type),type="s",xlab="Time in Days",ylab="Pollutants in Ug/m3" ),
             )
             
      )
      
    })
    
    ####################################### CALCULATE Tab ###########################################  
    output$sname=renderText({
      switch(input$season,
             "summer_data" =  paste("SUMMER"),
             "winter_data" = paste("WINTER")
      )
    })
    
    output$minm =renderText({
      y=get(input$type)
      min(y)
      
    })
    
    output$maxm =renderText({
      y=get(input$type)
      max(y)
      
    })
    
    output$mean =renderText({
      y=get(input$type)
      mean(y)
      
    })
    
    output$variance = renderText({
      y=get(input$type)
      var(y)
    })
    
    ######## correlation calculation ########
    output$c1 = renderUI({
      
      selectInput("cor1","SELECT THE POLLUTANT",choices = dataset())
      
    })
    
    output$c2 = renderUI({
      
      selectInput("cor2","SELECT THE POLLUTANT",choices = dataset())
      
    })
    
    output$corr =renderText({
      x=get(input$cor1)
      y=get(input$cor2)
      cor(x,y)
     
    })
   
    ######## Significance level((p<0.001) ########
    output$sig = renderText({
      x=get(input$cor1)
      y=get(input$cor2)
      cor.test(x,y)$p.value 
      
    })
    ################################## T Test Analysis Tab #####################################
    output$t1 = renderUI({
      
      selectInput("test1","SELECT THE POLLUTANT",choices = names(summer_data[,c(2,3,4,5,6)]))
      
    })
    
    output$t2 = renderUI({
      
      selectInput("test2","SELECT THE POLLUTANT",choices = names(winter_data[,c(2,3,4,5,6)]))
      
    })
    output$tvalue = renderText({
      #attach(get(winter_data))
      x=get(input$test1)
      y=get(input$test2)
       t.test(x,y)$statistic
      
    }) 
    
    output$dfmean= renderText({
       x=get(input$test1)
      y=get(input$test2)
       mean(x)-mean(y)
    })
    
    # ######## Significance level((p<0.001) ########
    # output$sig1 = renderText({
    #   x=get(input$test1)
    #   y=get(input$test2)
    #   p=mean(x)
    #   q=mean(y)
    #   cor.test(p,q)$p.value 
    #   
    # })
    ################################# Query's And Abstract View Tab ##############################
   
    #making a multiplot function
    
    multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
      library(grid)
      
      # Make a list from the ... arguments and plotlist
      plots <- c(list(...), plotlist)
      
      numPlots = length(plots)
      
      # If layout is NULL, then use 'cols' to determine layout
      if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
      }
      
      if (numPlots==1) {
        print(plots[[1]])
        
      } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
          # Get the i,j matrix positions of the regions that contain this subplot
          matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
          
          print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                          layout.pos.col = matchidx$col))
        }
      }
    }
    
     p1 = ggplot(smelt, aes(x=s_date, y=value, fill=variable,colour=variable)) + geom_line(stat='identity', position='dodge')+xlab("Time in Days")+ylab("Pollutants in Ug/m3")
    
    p2 = ggplot(wmelt, aes(x=w_date, y=value, fill=variable,colour=variable)) + geom_line(stat='identity', position='dodge')+xlab("Time in Days")+ylab("Pollutants in Ug/m3")
    output$abview = renderPlot({
      #par(mfrow = c(2, 1)) 
      #ggplot(smelt, aes(x=s_date, y=value, fill=variable,colour=variable)) + geom_line(stat='identity', position='dodge')+xlab("Time in Days")+ylab("Pollutants in Ug/m3")
      #ggplot(wmelt, aes(x=w_date, y=value, fill=variable,colour=variable)) + geom_line(stat='identity', position='dodge')+xlab("Time in Days")+ylab("Pollutants in Ug/m3")
    multiplot(p1,p2,cols = 2)
      })
    #######################################Summary Tab############################################
    # sum = reactive({
    #   switch(input$season,
    #          "summer_data" =  summary(summer_data),
    #          "winter_data" = summary(winter_data)
    #   )
    # })
    # output$summary = renderTable(
    #  data.frame(sum)
    # )
    ##########################################################################################################
  }
)