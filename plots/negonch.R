library(tidyverse)
negonch <- read_excel("data/negonch.xlsx", na = "0")

############################################### 
## Digital Skills  ----  
###############################################.    
# Create a "data_source" reactive variable
data_digital <- reactive({

    data_digital <- negonch %>% 
      gather("Frequency", "Value", "No" ,"Yes", "Prefer not to say" )

  return(data_digital)
})






# Already inside server
output$pageStub <- renderUI(fluidPage(

   # Application title
  
   titlePanel(h1("Other negative online experiences  ", align = "center")),



wellPanel(style = "background: #ffffff", 
        plotlyOutput("negonch")
      )


))




output$negonch <- renderPlotly({
  

      fig <- plot_ly(data = data_digital(),
                     x = ~Value, 
                     y = ~Answers, 
                     type = 'bar', 
                     color = ~Frequency,
                     colors = brewer.pal(n = 3, "Paired")) %>%
        layout(title= "In the PAST YEAR, has any of the following happened to you on the internet?",
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
