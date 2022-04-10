library(tidyverse)
digskillch <- read_excel("data/digskillch.xlsx", na = "0")

############################################### 
## Digital Skills  ----  
###############################################.    
# Create a "data_source" reactive variable
data_digital <- reactive({

    data_digital <- digskillch %>% 
      gather("Frequency", "Value", "Not true for me","A bit true for me",   "Fairly true for me" ,  "Very true for me",
             "I dont know what this activity/ thing means","Dont know","Refusal"  )

  return(data_digital)
})






# Already inside server
output$pageStub <- renderUI(fluidPage(

   # Application title
  
   titlePanel(h1("Children's digital skills", align = "center")),



wellPanel(style = "background: #ffffff", 
        plotlyOutput("digskillch")
      )


))




output$digskillch <- renderPlotly({
  

      fig <- plot_ly(data = data_digital(),
                     x = ~Value, 
                     y = ~Activity, 
                     type = 'bar', 
                     color = ~Frequency,
                     colors = brewer.pal(n = 3, "Paired")) %>%
        layout(title= "Children's confidence in digital skills",
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
