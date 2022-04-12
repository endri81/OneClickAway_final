library(tidyverse)
conpar <- read_excel("data/conpar.xlsx", na = "0")

############################################### 
## Digital Skills  ----  
###############################################.    
# Create a "data_source" reactive variable
data_digital <- reactive({
  
  data_digital <- conpar %>% 
    gather("Frequency", "Value", "No" ,"Yes", "Refuse to answer" )
  
  return(data_digital)
})






# Already inside server
output$pageStub <- renderUI(fluidPage(
  
  # Application title
  
  titlePanel(h1("Parental Technical mediation", align = "center")),
  
  
  
  wellPanel(style = "background: #ffffff", 
            plotlyOutput("conpar")
  )
  
  
))




output$conpar <- renderPlotly({
  
  
  fig <- plot_ly(data = data_digital(),
                 x = ~Value, 
                 y = ~Question, 
                 type = 'bar', 
                 color = ~Frequency,
                 colors = brewer.pal(n = 3, "Paired")) %>%
    layout(title= "Do you (or other parent/carer) make use of any of the following?",
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
