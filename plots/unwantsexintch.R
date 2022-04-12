library(tidyverse)
unwantsexintch <- read_excel("data/unwantsexintch.xlsx", na = "0")

############################################### 
## Digital Skills  ----  
###############################################.    
# Create a "data_source" reactive variable
data_digital <- reactive({
  
  data_digital <- unwantsexintch %>% 
    gather("Frequency", "Value", "No" ,"Yes", "Prefer not to say" )
  
  return(data_digital)
})






# Already inside server
output$pageStub <- renderUI(fluidPage(
  
  # Application title
  
  titlePanel(h1("Children's unwanted sexual experiences", align = "center")),
  
  
  
  wellPanel(style = "background: #ffffff", 
            plotlyOutput("unwantsexintch")
  )
  
  
))




output$unwantsexintch <- renderPlotly({
  
  
  fig <- plot_ly(data = data_digital(),
                 x = ~Value, 
                 y = ~Answer, 
                 type = 'bar', 
                 color = ~Frequency,
                 colors = brewer.pal(n = 3, "Paired")) %>%
    layout(title= "In the PAST YEAR, have any of these happened to you on the internet at least once?",
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
