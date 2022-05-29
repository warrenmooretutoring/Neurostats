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
library(shinyjs)
library(tmaptools)

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$script(src = "script.js"),
  mainPanel(
    includeCSS("www/style.css"),
    includeHTML("index.html")
  ),
  plotOutput(outputId = "ErrorbarPlot")
)

# Define server logic required to make plot
server <- function(input, output){
  output$ErrorbarPlot <- renderPlot({
    # generate dataframe holding input data   

    
 #   if (input$errorBarType == "95%CI") {
#      error <- c(input$IV1_level1_Upper_1, input$IV1_level1_Lower_1)
#    } else {
#      error <- c(input$IV1_level1_Upper_1,input$IV1_level1_Lower_1)
#    } 
  
      if (input$errorBarType == "95%CI") {
        df<-data.frame(Mean=c(input$IV1_level1_mean, input$IV1_level2_mean), 
                       ymaxcol = c(input$IV1_level1_Upper, input$IV1_level2_Upper), 
                       ymincol = c(input$IV1_level1_Lower, input$IV1_level2_Lower),
                       Category=c(input$IV1_level1_name, input$IV1_level2_name))  
           } else {
             df<-data.frame(Mean=c(input$IV1_level1_mean, input$IV1_level2_mean), 
                            ymaxcol = c(input$IV1_level1_Upper, input$IV1_level2_Upper), 
                            ymincol = c(input$IV1_level1_Upper, input$IV1_level2_Upper),
                            Category=c(input$IV1_level1_name, input$IV1_level2_name)) 
        } 

    # plot in graph
    (p <- ggplot(df, aes(x=Category, y=Mean, fill = Category)) + 
        geom_bar(stat="identity") +
        geom_errorbar(aes(ymin=Mean - ymincol, ymax=Mean + ymaxcol), width=.2,
                      position=position_dodge(0.05)) +
        labs(title = paste("Figure", input$fig_number),
             subtitle = input$fig_subtitle,
           #  caption = paste("italic('Note.') Error bars indicate ", input$errorBarType,". ", input$fig_caption)) +
          #  caption = paste("italic('Note.') Error bars indicate ", input$errorBarType,". ", input$fig_caption)) +
        caption = expression(paste(italic("Note. "), "Error bars indicate ", input$errorBarType, "test text"))) +
        xlab(input$IV_name) +
        ylab(input$DV_name) +
        ylab(input$IV1_Level1_Color) +
        theme_classic() +
        theme(plot.title = element_text(face = "bold", size = 20),
              plot.subtitle = element_text(size = 16),
              plot.caption = element_text(size = 12, hjust = 0),
              axis.title.x = element_text(face = "bold", size = 14),
              axis.title.y = element_text(face = "bold", size = 14),
              axis.text = element_text(size = 12))) 
    #Toggle watermark - add watermark to plot when checkbox ticked, else load plot without
    if (input$TogglePlotWatermark == 1) {
      p + annotate("text", x = c(1, 2), y = c(60, 40), 
                   label = c("NeuroStats.co.uk") , color="blue", alpha=.2,
                   size=15 , angle=35, fontface="bold") } else {
                     p
                   }
    #Change plot bar colours using html colour picker
    if (input$barColor == "orange") {
      p + scale_fill_manual(values = c("#FDDCBA", "#EC6E0F"))
    } else {
     p + scale_fill_brewer(palette = "BuPu") 
    }
    
})
}

#Show colour pallets
#tmaptools::palette_explorer()

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