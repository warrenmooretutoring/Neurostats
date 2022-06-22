
library(shiny)
library(tidyverse)
library(magrittr)
library(RColorBrewer)
library(shinyjs)
library(tmaptools)

#TO DO
# JavaScript to copy over all desired parameters to actual stimulus table (except duration which needs to be calculated)
# Javescript not being called from 'includeScript' - does this need to be placed in 'www' folder?
# 
#


# Define UI for application that draws a histogram
ui <- fluidPage(
  mainPanel(
 #   includeCSS("../Shiny R/www/style.css"),
    includeHTML("QSTLab_stimulusBuilder.html"),
    includeScript("script.js")
  ),
  plotOutput(outputId = "stimulusPlot")
)

# Define server logic required to make plot
server <- function(input, output){
  output$stimulusPlot <- renderPlot({
    # generate dataframe holding input data 
    
  # Element 1 timep oints
    E1_ramp_time = input$Desired_E1_Offset
    E1_hold_time = input$Desired_E1_Offset + sqrt((input$Desired_E1_Baseline - input$Desired_E1_Temp)^2) / input$Desired_E1_Speed
    E1_return_time = input$Desired_E1_Offset + input$Desired_E1_Duration 
    E1_off_time = E1_return_time + sqrt((input$Desired_E1_Baseline - input$Desired_E1_Temp)^2) / input$Desired_E1_Return
  
  # Element 2 time points
      
  # Element 3 time points
      
  # Element 4 time points
      
  # Element 5 time points

#    stimulusTimeline2 <- data.frame(
#      stage = c("ramp", "hold", "return", "off"),
#      time = c(E1_ramp_time, E1_hold_time, E1_return_time, E1_off_time),
#      temp = c(input$Desired_E1_Baseline, input$Desired_E1_Temp, input$Desired_E1_Temp, input$Desired_E1_Baseline),
#      element=c("1", "1", "1", "1")
#      #element=c("1","1","1","1","1","2","2","2","2","2","3","3","3","3","3","4","4","4","4","4","5","5","5","5","5")
#    )

  
    stimulusTimeline <- data.frame(
      stage=c("ramp", "hold", "return", "off"),
      time=c(E1_ramp_time, E1_hold_time, E1_return_time, E1_off_time),
      temp = c(input$Desired_E1_Baseline, input$Desired_E1_Temp, input$Desired_E1_Temp, input$Desired_E1_Baseline),
      element=c("1","1","1","1")
    )

    # plot in graph
    (p <- ggplot(stimulusTimeline, aes(x=time, y=temp, group=element)) +
        geom_line(aes(color=element))+
        geom_point(aes(color=element))+
        theme_classic()+
        xlab("Temperature (C)") +
        ylab("Time (s)") +
        theme_classic() +
        theme(plot.title = element_text(face = "bold", size = 20),
              plot.subtitle = element_text(size = 16),
              plot.caption = element_text(size = 12, hjust = 0),
              axis.title.x = element_text(face = "bold", size = 14),
              axis.title.y = element_text(face = "bold", size = 14),
              axis.text = element_text(size = 12))) 
    

  
  })
}

#Show colour pallets
#tmaptools::palette_explorer()
# get_brewer_pal("Blues", n = 7)



# Run the application 
shinyApp(ui = ui, server = server)

