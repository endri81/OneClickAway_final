library(tidyverse)
restrictpar <- read_excel("data/restrictpar.xlsx", na = "0")

############################################### 
## Digital Skills  ----  
###############################################.    
# Create a "data_source" reactive variable
data_digital <- reactive({
  
  data_digital <- restrictpar %>% 
    gather("Frequency", "Value", "Can do this anytime","Can only do this with permission or supervision", "Can never do this",                              
           "I dont know if my child performs this activity online", "Dont know", "Refusal")                                        
  
  return(data_digital)
})






# Already inside server
output$pageStub <- renderUI(fluidPage(
  
  # Application title
  
  titlePanel(h1("Restrictive mediation according to parents ", align = "center")),
  
  
  
  wellPanel(style = "background: #ffffff", 
            plotlyOutput("restrictpar")
  )
  
  
))




output$restrictpar <- renderPlotly({
  
  
  fig <- plot_ly(data = data_digital(),
                 x = ~Value, 
                 y = ~Question, 
                 type = 'bar', 
                 color = ~Frequency,
                 colors = brewer.pal(n = 3, "Paired")) %>%
    layout(title= "For each of these actions, please indicate if you CURRENTLY let your child perform them whenever s/he wants, <br /> or let her/him perform them but only with your permission or supervision, <br /> or you never let her/him perform them",
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
