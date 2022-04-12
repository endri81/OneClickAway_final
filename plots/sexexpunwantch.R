library(tidyverse)
sexexpunwantch <- read_excel("data/sexexpunwantch.xlsx", na = "0")

############################################### 
## Digital Skills  ----  
###############################################.    
# Create a "data_source" reactive variable
data_digital <- reactive({
  
  data_digital <- sexexpunwantch %>% 
    gather("Frequency", "Value", "No" ,"Yes", "Dont know", "Prefer not to say" )
  
  return(data_digital)
})






# Already inside server
output$pageStub <- renderUI(fluidPage(
  
  # Application title
  
  titlePanel(h1("Children's unwanted sexual experiences", align = "center")),
  
  
  
  wellPanel(style = "background: #ffffff", 
            plotlyOutput("sexexpunwantch")
  )
  
  
))




output$sexexpunwantch <- renderPlotly({
  
  
  fig <- plot_ly(data = data_digital(),
                 x = ~Value, 
                 y = ~Answer, 
                 type = 'bar', 
                 color = ~Frequency,
                 colors = brewer.pal(n = 3, "Paired")) %>%
    layout(title= "In the PAST YEAR , have any of these happened to you on real life at least once?",
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
