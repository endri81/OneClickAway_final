library(tidyverse)
negconch <- read_excel("data/negconch.xlsx", na = "0")

############################################### 
## Digital Skills  ----  
###############################################.    
# Create a "data_source" reactive variable
data_digital <- reactive({

    data_digital <- negconch %>% 
      gather("Frequency", "Value", "No" ,"Yes", "Dont know", "Prefer not to say" )

  return(data_digital)
})






# Already inside server
output$pageStub <- renderUI(fluidPage(

   # Application title
  
   titlePanel(h1("Potentially negative user-generated content by children ", align = "center")),



wellPanel(style = "background: #ffffff", 
        plotlyOutput("negconch")
      )


))




output$negconch <- renderPlotly({
  

      fig <- plot_ly(data = data_digital(),
                     x = ~Value, 
                     y = ~Means, 
                     type = 'bar', 
                     color = ~Frequency,
                     colors = brewer.pal(n = 3, "Paired")) %>%
        layout(title= "In the PAST YEAR, have you seen websites or online discussions where people talk about ?",
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
