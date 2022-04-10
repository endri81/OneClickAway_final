library(tidyverse)
freqintch <- read_excel("data/freqintch.xlsx", na = "0")

############################################### 
## Digital Skills  ----  
###############################################.    
# Create a "data_source" reactive variable
data_digital <- reactive({

    data_digital <- freqintch %>% 
      gather("Frequency", "Value","Never","Hardly ever","At least every month",   
             "At least every week",  "Daily or almost daily",  "Several times each day", "Almost all the time" ,  
             "Dont know", "Refusal")

  return(data_digital)
})






# Already inside server
output$pageStub <- renderUI(fluidPage(

   # Application title
  
   titlePanel(h1("Frequency of Internet use through different devices for children", align = "center")),



wellPanel(style = "background: #ffffff", 
        plotlyOutput("freqintch")
      )


))




output$freqintch <- renderPlotly({
  

      fig <- plot_ly(data = data_digital(),
                     x = ~Device, 
                     y = ~Value, 
                     type = 'bar', 
                     color = ~Frequency,
                     colors = brewer.pal(n = 3, "Paired")) %>%
        layout(title= "When you use Internet, how often do you use any of these to go online?",
               yaxis=list(title = "", standoff = 20L),
               xaxis=list(title = "Base: Total Target population (n = 1000 children)"),
               barmode= "stack") %>% config(displaylogo = FALSE,
                                            modeBarButtonsToRemove = list(
                                              'sendDataToCloud',
                                              'autoScale2d',
                                              'resetScale2d',
                                              'hoverClosestCartesian',
                                              'hoverCompareCartesian'
                                            )) %>% layout(yaxis= list(showticklabels = FALSE))
      
   
})
