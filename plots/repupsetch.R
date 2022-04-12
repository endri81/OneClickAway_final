library(tidyverse)
repupsetch <- read_excel("data/repupsetch.xlsx", na = "0")

############################################### 
## Digital Skills  ----  
###############################################.    
# Create a "data_source" reactive variable
data_digital <- reactive({
  # Return the appropriate data source depending on
  # the chosen radio button
  if (input$ind_repupsetch == "age") {
    data_dig <- repupsetch %>% select("Answer", "9 - 11", "12 - 14", "15 - 17" ) %>%
      gather("age_group", "age_value", "9 - 11", "12 - 14", "15 - 17")
    
  } else if (input$ind_repupsetch == "gender") {
    data_dig <- repupsetch %>% select("Answer", Male, Female) %>%
      gather("gender", "gender_value", "Male", "Female")  
    
  }
  
  else if (input$ind_repupsetch == "total") {
    data_dig <- repupsetch %>% select("Answer", "Total")
    
  }
  return(data_dig)
})






# Already inside server
output$pageStub <- renderUI(fluidPage(

   # Application title
  
   titlePanel(h1("Children's self-reported bothered or upset in the last year", align = "center")),

   column(3,
          wellPanel(style = "background: #ffffff", 
            pickerInput(
              inputId = "ind_repupsetch",
              label = "Select metrics", 
              choices = c("Age" = "age", "Gender" = "gender", "Total" = "total"),
              options = list(
                style = "btn-primary")
          )       
   )),
   
   column(3),
   
   column(9,

wellPanel(style = "background: #ffffff", 
        plotlyOutput("repupsetch")
      )
   )

))




output$repupsetch <- renderPlotly({
  
    
    if (input$ind_repupsetch == "age")
    {
      fig <- plot_ly(data = data_digital(),
                     x = ~age_value, 
                     y = ~Answer, 
                     type = 'bar', 
                     color = ~age_group,
                     colors = brewer.pal(n = 3, "Paired")) %>%
        layout(title= "The last time something like that happened online, how did you feel about it?",
               yaxis=list(title = "", standoff = 20L),
               xaxis=list(title = "Base: Children that reported that something happened online that bothered or upset them in some way (n=140 children)"),
               barmode= "stack") %>% config(displaylogo = FALSE,
                                            modeBarButtonsToRemove = list(
                                              'sendDataToCloud',
                                              'autoScale2d',
                                              'resetScale2d',
                                              'hoverClosestCartesian',
                                              'hoverCompareCartesian'
                                            )) 
      
    }         
    else if (input$ind_repupsetch == "gender")
    {
      fig <- plot_ly(data = data_digital(),
                     x = ~gender_value, 
                     y = ~Answer, 
                     type = 'bar', 
                     color = ~gender,
                     colors = brewer.pal(n = 3, "Paired")) %>%
        layout(title= "The last time something like that happened online, how did you feel about it?",
               yaxis=list(title = "", standoff = 20L),
               xaxis=list(title = "Base: Children that reported that something happened online that bothered or upset them in some way (n=140 children)"),
               barmode= "stack") %>% config(displaylogo = FALSE,
                                            modeBarButtonsToRemove = list(
                                              'sendDataToCloud',
                                              'autoScale2d',
                                              'resetScale2d',
                                              'hoverClosestCartesian',
                                              'hoverCompareCartesian'
                                            )) 
    }
    else  {
      fig <- plot_ly(data = data_digital(),
                     x = ~Total, 
                     y = ~Answer, 
                     type = 'bar', 
                     color = ~Total,
                     colors = brewer.pal(n = 3, "Paired")) %>%
        layout(title= "The last time something like that happened online, how did you feel about it?",
               yaxis=list(title = "", standoff = 20L),
               xaxis=list(title = "Base: Children that reported that something happened online that bothered or upset them in some way (n=140 children)"),
               barmode= "stack") %>% config(displaylogo = FALSE,
                                            modeBarButtonsToRemove = list(
                                              'sendDataToCloud',
                                              'autoScale2d',
                                              'resetScale2d',
                                              'hoverClosestCartesian',
                                              'hoverCompareCartesian'
                                            ))
 
    }
  
  
})
