library(tidyverse)
placesintch <- read_excel("data/placesintch.xlsx", na = "0")

############################################### 
## Digital Skills  ----  
###############################################.    
# Create a "data_source" reactive variable
data_digital <- reactive({

    data_digital <- placesintch %>% 
      gather("Frequency", "Value","Never","Hardly ever","At least every month",   
             "At least every week",  "Daily or almost daily",  "Several times each day", "Almost all the time" ,  
             "Dont know", "Refusal")

  return(data_digital)
})






# Already inside server
output$pageStub <- renderUI(fluidPage(

   # Application title
  
   titlePanel(h1("Places of internet use by place for children", align = "center")),



wellPanel(style = "background: #ffffff", 
        plotlyOutput("placesintch")
      )


))




output$placesintch <- renderPlotly({
  

      fig <- plot_ly(data = data_digital(),
                     x = ~Value, 
                     y = ~Place, 
                     type = 'bar', 
                     color = ~Frequency,
                     colors = brewer.pal(n = 3, "Paired")) %>%
        layout(title= "How often do you go online or use the internet at the following places?",
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
