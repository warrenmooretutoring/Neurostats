#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(magrittr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Included Content"),
  mainPanel(
    includeHTML("index.html")
  )
)

# Define server logic required to make plot
server <- function(input, output){
    output$ErrorbarPlot <- renderPlot({
        # generate dataframe holding input data          
        df<-data.frame(Mean=c(input$helloValue, input$helloValue2),
                       sd=c(input$sd1,input$sd2),
                       Category=c(input$group1label, input$group2label))
        
        # plot in graph
        (p <- ggplot(df, aes(x=Category, y=Mean)) + 
            geom_point()+
            geom_errorbar(aes(ymin=Mean-sd, ymax=Mean+sd), width=.2,
                          position=position_dodge(0.05)) +
            labs(title = paste("Figure", input$fign),
                 subtitle = input$subtitle,
                 caption = paste("Note. Error bars indicate ", input$errorbars, ". ", input$caption)) +
            xlab(input$xlabel) +
            ylab(input$ylabel) +
            theme_classic() +
            theme(plot.title = element_text(face = "bold", size = 20),
                  plot.subtitle = element_text(size = 16),
                  plot.caption = element_text(size = 12),
                  axis.title.x = element_text(face = "bold", size = 14),
                  axis.title.y = element_text(face = "bold", size = 14),
                  axis.text = element_text(size = 12))) 
          
    })
    
}


# add download button
# output$downloadData <- downloadHandler(
#   filename = function() {
#     paste('data-', Sys.Date(), '.png', sep='')
#   },
#   content = function(p) {
#     ggsave(plot = p, filename = plot, device = "png")
#   }
# )

# Run the application 
shinyApp(ui = ui, server = server)
