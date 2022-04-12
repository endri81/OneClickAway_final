library(tidyverse)
afterconch <- read_excel("data/afterconch.xlsx", na = "0")

############################################### 
## Digital Skills  ----  
###############################################.    
# Create a "data_source" reactive variable
data_digital <- reactive({
  
  data_digital <- afterconch %>% 
    gather("Frequency", "Value","Never","Hardly ever","Sometimes","Often","Very often","Dont know - dont read", "Refusal")
  
  return(data_digital)
})






# Already inside server
output$pageStub <- renderUI(fluidPage(
  
  # Application title
  
  titlePanel(h1("Parental monitoring according to children ", align = "center")),
  
  
  
  wellPanel(style = "background: #ffffff", 
            plotlyOutput("afterconch")
  )
  
  
))




output$afterconch <- renderPlotly({
  
  
  fig <- plot_ly(data = data_digital(),
                 x = ~Value, 
                 y = ~Answer, 
                 type = 'bar', 
                 color = ~Frequency,
                 colors = brewer.pal(n = 3, "Paired")) %>%
    layout(title= "When you use the internet, how often does your parent/carer check the following things afterwards?",
           yaxis=list(title = "", standoff = 20L),
           xaxis=list(title = "Base: Total target population"),
           barmode= "stack") %>% config(displaylogo = FALSE,
                                        modeBarButtonsToRemove = list(
                                          'sendDataToCloud',
                                          'autoScale2d',
                                          'resetScale2d',
                                          'hoverClosestCartesian',
                                          'hoverCompareCartesian'
                                        )) 
  
  
})
