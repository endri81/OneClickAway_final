library(tidyverse)
treatlastpar <- read_excel("data/treatlastpar.xlsx", na = "0")

############################################### 
## Digital Skills  ----  
###############################################.    
# Create a "data_source" reactive variable
data_digital <- reactive({
  
  data_digital <- treatlastpar %>% 
    gather("Frequency", "Value", "No" ,"Yes", "Dont know", "Prefer not to say" )
  
  return(data_digital)
})






# Already inside server
output$pageStub <- renderUI(fluidPage(
  
  # Application title
  
  titlePanel(h1("Parental perception of child's online risk", align = "center")),
  
  
  
  wellPanel(style = "background: #ffffff", 
            plotlyOutput("treatlastpar")
  )
  
  
))




output$treatlastpar <- renderPlotly({
  
  
  fig <- plot_ly(data = data_digital(),
                 x = ~Value, 
                 y = ~Answer, 
                 type = 'bar', 
                 color = ~Frequency,
                 colors = brewer.pal(n = 3, "Paired")) %>%
    layout(title= "As far as you are aware, in the past year, have any of these things happened to your child on the internet at least once?",
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
