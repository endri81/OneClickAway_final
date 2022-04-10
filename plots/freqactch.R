library(tidyverse)
freqactch <- read_excel("data/freqactch.xlsx", na = "0")

############################################### 
## Digital Skills  ----  
###############################################.    
# Create a "data_source" reactive variable
data_digital <- reactive({

    data_digital <- freqactch %>% 
      gather("Frequency", "Value","Never","Hardly ever",  
             "At least every week",  "Daily or almost daily",  "Several times each day", "Almost all the time" ,  "I dont know what this activity/ thing means",
             "Dont know", "Refusal")

  return(data_digital)
})






# Already inside server
output$pageStub <- renderUI(fluidPage(

   # Application title
  
   titlePanel(h1("Frequency of activities practiced weekly or more often by children", align = "center")),



wellPanel(style = "background: #ffffff", 
        plotlyOutput("freqactch")
      )


))




output$freqactch <- renderPlotly({
  

      fig <- plot_ly(data = data_digital(),
                     x = ~Value, 
                     y = ~Activity, 
                     type = 'bar', 
                     color = ~Frequency,
                     colors = brewer.pal(n = 3, "Paired")) %>%
        layout(title= "How often you have done these things online in the past month?",
               yaxis=list(title = "", standoff = 20L),
               xaxis=list(title = "Base: Total Target population (n = 1000 children)"),
               barmode= "stack") %>% config(displaylogo = FALSE,
                                            modeBarButtonsToRemove = list(
                                              'sendDataToCloud',
                                              'autoScale2d',
                                              'resetScale2d',
                                              'hoverClosestCartesian',
                                              'hoverCompareCartesian'
                                            )) 
      
   
})
