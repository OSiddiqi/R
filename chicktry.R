#chicktry
output$dietPlot <- renderPlot({
  
  diet <- input$diet
  
  if (diet == "All") {
    graphData <- ChickWeight %>% 
      group_by(Diet,Time) %>% 
      summarise(MeanWeight = mean(weight))
    
    ggplot(graphData,aes(x=Time, y=MeanWeight, color=Diet)) +
      geom_line() +
      geom_point() +
      ggtitle("Chick Weight by Diet") +
      ylab(expression("Mean Weight")) +
      scale_colour_discrete(name = "Diet", label = c("1","2","3","4"))# Generate a summary of the data 
    
  }
  else {
    graphData <- ChickWeight %>% 
      filter(Diet == diet)
    
    ggplot(graphData,aes(x=Time, y=weight, color=Chick)) +
      geom_line() +
      geom_point() +
      ggtitle("Chick Weight for Diet ") +
      ylab(expression("Mean Weight")) 
    
    
  }
}