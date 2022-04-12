library(tidyverse)
afterconpar <- read_excel("data/afterconpar.xlsx", na = "0")

############################################### 
## Digital Skills  ----  
###############################################.    
# Create a "data_source" reactive variable
data_digital <- reactive({
  
  data_digital <- afterconpar %>% 
    gather("Frequency", "Value","Never","Hardly ever","Sometimes","Often","Very often","Dont know - dont read", "Refusal")
  
  return(data_digital)
})






# Already inside server
output$pageStub <- renderUI(fluidPage(
  
  # Application title
  
  titlePanel(h1("Monitoring for parents", align = "center")),
  
  
  
  wellPanel(style = "background: #ffffff", 
            plotlyOutput("afterconpar")
  )
  
  
))




output$afterconpar <- renderPlotly({
  
  
  fig <- plot_ly(data = data_digital(),
                 x = ~Value, 
                 y = ~Answer, 
                 type = 'bar', 
                 color = ~Frequency,
                 colors = brewer.pal(n = 3, "Paired")) %>%
    layout(title= "When your child uses the internet, how often do you (or other parent/carer) check the following things afterwards?",
           yaxis=list(title = "", standoff = 20L),
           xaxis=list(title = "Base: those who use internet (68% of target population)"),
           barmode= "stack") %>% config(displaylogo = FALSE,
                                        modeBarButtonsToRemove = list(
                                          'sendDataToCloud',
                                          'autoScale2d',
                                          'resetScale2d',
                                          'hoverClosestCartesian',
                                          'hoverCompareCartesian'
                                        )) 
  
  
})
