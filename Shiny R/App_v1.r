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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Link css style sheet
   includeCSS("www/style.css"),
  
 
    # Application title
    titlePanel(h1("T-test plot")),
    
    # label input
    textInput("ylabel", label = h2("Dependent variable"), value = "Reaction time"),
    textInput("xlabel", label = h2("Independent variable"), value = "Drink type"),

    textInput("group1label", label = h2("Level 1"), value = "Coffee"),
    textInput("group2label", label = h2("Level 2"), value = "Water"),
    
    # Numberic input 
    numericInput("mean1", label = h2("Mean value for group 1"), value = 1),
    numericInput("mean2", label = h2("Mean value for group 1"), value = 1.5),

    numericInput("sd1", label = h2("standard deviation for group 1"), value = 0.016),
    numericInput("sd2", label = h2("standard deviation for group 2"), value = 0.028),
    
    # error bars
    radioButtons("errorbars", label = h2("Error bars indicate:"),
                 choices = list("Standard deviation" = "SD", "Standard Error" = "SE", "95% Confidence Interval" = "CI"), 
                 selected = 1),

    textInput("fign", label = h2("Figure number"), value = "1"),
    textInput("subtitle", label = h2("Figure description"), value = "Reaction time as a function of drink type"),
    textInput("caption", label = h2("Figure notes"), value = "No significant difference in reaction time was found between drink type coffee and water"),
    
    hr(),
    fluidRow(column(3, verbatimTextOutput("value"))),
    
    # Show a plot of the generated distribution
    mainPanel(plotOutput("ErrorbarPlot")),
    
  #  downloadButton(outputId, label = "Download")
)

# Define server logic required to make plot
server <- function(input, output){
    output$ErrorbarPlot <- renderPlot({
        # generate dataframe holding input data          
        df<-data.frame(Mean=c(input$mean1, input$mean2),
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
