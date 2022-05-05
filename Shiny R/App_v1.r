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
library(RColorBrewer)

# Define UI for application that draws a histogram
ui <- fluidPage(
  mainPanel(
    includeHTML("index.html")
  ),
  plotOutput(outputId = "ErrorbarPlot")
 )

# Define server logic required to make plot
server <- function(input, output){
    output$ErrorbarPlot <- renderPlot({
        # generate dataframe holding input data          
        df<-data.frame(Mean=c(input$IV1_level1_mean, input$IV1_level2_mean),
                       sd=c(input$IV1_level1_sd,input$IV1_level2_sd),
                       Category=c(input$IV1_level1_name, input$IV1_level2_name))
        
        
        # plot in graph
        (p <- ggplot(df, aes(x=Category, y=Mean)) + 
            geom_bar(stat="identity") +
            scale_colour_brewer(palette = "YlOrRd", direction = - 1) + 
            scale_fill_brewer(palette = "BuPu") +
            geom_errorbar(aes(ymin=Mean-sd, ymax=Mean+sd), width=.2,
                          position=position_dodge(0.05)) +
            labs(title = paste("Figure", input$fig_number),
                 subtitle = input$fig_subtitle,
                 caption = paste("Note. Error bars indicate", input$errorBarType,". ", input$fig_caption)) +
            xlab(input$IV_name) +
            ylab(input$DV_name) +
           # theme_classic() +
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
