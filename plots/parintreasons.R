library(tidyverse)
parentintusage <- read_excel("data/parintreasons.xlsx")
# Already inside server
output$pageStub <- renderUI(fluidPage(

   # Application title
   titlePanel("When you are unable to access the internet, what are the reasons?"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),

      # Show a plot of the generated distribution
      mainPanel(
        plotlyOutput("parentintusage")
      )
   )
))


output$parentintusage <- renderPlotly({
  
  fig <- plot_ly(data = parentintusage,
                 x = ~Question, 
                 y = ~Total, 
                 type = 'bar', 
                 color = ~Total,
                 colors = brewer.pal(n = 3, "Paired")) %>%
    layout(title="Reasons for limited access to the internet for parents or Reasons for parents limited access to the internet ",
           yaxis=list(title = "Websites or apps"),
           xaxis=list(title = "Responders"),
           barmode= "stack") %>% config(displaylogo = FALSE,
                                        modeBarButtonsToRemove = list(
                                          'sendDataToCloud',
                                          'autoScale2d',
                                          'resetScale2d',
                                          'hoverClosestCartesian',
                                          'hoverCompareCartesian'))
})
