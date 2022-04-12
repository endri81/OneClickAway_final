library(tidyverse)
restrictch <- read_excel("data/restrictch.xlsx", na = "0")

############################################### 
## Digital Skills  ----  
###############################################.    
# Create a "data_source" reactive variable
data_digital <- reactive({
  
  data_digital <- restrictch %>% 
    gather("Frequency", "Value", "Can do this anytime","Can only do this with permission or supervision", "Can never do this",                              
           "I never do this on the internet", "Dont know", "Refusal")                                        
  
  return(data_digital)
})






# Already inside server
output$pageStub <- renderUI(fluidPage(
  
  # Application title
  
  titlePanel(h1("Restrictive parental mediation according to children ", align = "center")),
  
  
  
  wellPanel(style = "background: #ffffff", 
            plotlyOutput("restrictch")
  )
  
  
))




output$restrictch <- renderPlotly({
  
  
  fig <- plot_ly(data = data_digital(),
                 x = ~Value, 
                 y = ~Question, 
                 type = 'bar', 
                 color = ~Frequency,
                 colors = brewer.pal(n = 3, "Paired")) %>%
    layout(title= "For each of these things, please indicate if your parent(s)/carer (s) CURRENTLY let you <br /> perform them whenever you want or let you do them but only with your parent'(s)/carer'(s) permission or supervision, <br /> or NEVER let you do them",
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
