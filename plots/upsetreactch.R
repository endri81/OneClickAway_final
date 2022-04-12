library(tidyverse)
upsetreactch <- read_excel("data/upsetreactch.xlsx", na = "0")

############################################### 
## Digital Skills  ----  
###############################################.    
# Create a "data_source" reactive variable
data_digital <- reactive({
  # Return the appropriate data source depending on
  # the chosen radio button
  if (input$ind_upsetreactch == "age") {
    data_dig <- upsetreactch %>% select("Answer", "9 - 11", "12 - 14", "15 - 17" ) %>%
      gather("age_group", "age_value", "9 - 11", "12 - 14", "15 - 17")
    
  } else if (input$ind_upsetreactch == "gender") {
    data_dig <- upsetreactch %>% select("Answer", Male, Female) %>%
      gather("gender", "gender_value", "Male", "Female")  
    
  }
  
  else if (input$ind_upsetreactch == "total") {
    data_dig <- upsetreactch %>% select("Answer", "Total")
    
  }
  return(data_dig)
})






# Already inside server
output$pageStub <- renderUI(fluidPage(

   # Application title
  
   titlePanel(h1("Reacting to something that bothered or upset children", align = "center")),

   column(3,
          wellPanel(style = "background: #ffffff", 
            pickerInput(
              inputId = "ind_upsetreactch",
              label = "Select metrics", 
              choices = c("Age" = "age", "Gender" = "gender", "Total" = "total"),
              options = list(
                style = "btn-primary")
          )       
   )),
   
   column(3),
   
   column(9,

wellPanel(style = "background: #ffffff", 
        plotlyOutput("upsetreactch")
      )
   )

))




output$upsetreactch <- renderPlotly({
  
    
    if (input$ind_upsetreactch == "age")
    {
      fig <- plot_ly(data = data_digital(),
                     x = ~age_value, 
                     y = ~Answer, 
                     type = 'bar', 
                     color = ~age_group,
                     colors = brewer.pal(n = 3, "Paired")) %>%
        layout(title= "The last time something happened online that bothered or upset you in some way, did you do any of these things afterwards?",
               yaxis=list(title = "", standoff = 20L),
               xaxis=list(title = "Base: If anything happened online that bothered or upset them in some way (14% of target population)"),
               barmode= "stack") %>% config(displaylogo = FALSE,
                                            modeBarButtonsToRemove = list(
                                              'sendDataToCloud',
                                              'autoScale2d',
                                              'resetScale2d',
                                              'hoverClosestCartesian',
                                              'hoverCompareCartesian'
                                            )) 
      
    }         
    else if (input$ind_upsetreactch == "gender")
    {
      fig <- plot_ly(data = data_digital(),
                     x = ~gender_value, 
                     y = ~Answer, 
                     type = 'bar', 
                     color = ~gender,
                     colors = brewer.pal(n = 3, "Paired")) %>%
        layout(title= "The last time something happened online that bothered or upset you in some way, did you do any of these things afterwards?",
               yaxis=list(title = "", standoff = 20L),
               xaxis=list(title = "Base: If anything happened online that bothered or upset them in some way (14% of target population)"),
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
        layout(title= "The last time something happened online that bothered or upset you in some way, did you do any of these things afterwards?",
               yaxis=list(title = "", standoff = 20L),
               xaxis=list(title = "Base: If anything happened online that bothered or upset them in some way (14% of target population)"),
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
