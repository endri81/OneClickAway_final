library(tidyverse)
protectch <- read_excel("data/protectch.xlsx", na = "0")

############################################### 
## Digital Skills  ----  
###############################################.    
# Create a "data_source" reactive variable
data_digital <- reactive({
  # Return the appropriate data source depending on
  # the chosen radio button
  if (input$ind_protectch == "age") {
    data_dig <- protectch %>% select("Answer", "9 - 11", "12 - 14", "15 - 17" ) %>%
      gather("age_group", "age_value", "9 - 11", "12 - 14", "15 - 17")
    
  } else if (input$ind_protectch == "gender") {
    data_dig <- protectch %>% select("Answer", Male, Female) %>%
      gather("gender", "gender_value", "Male", "Female")  
    
  }
  
  else if (input$ind_protectch == "total") {
    data_dig <- protectch %>% select("Answer", "Total")
    
  }
  return(data_dig)
})






# Already inside server
output$pageStub <- renderUI(fluidPage(

   # Application title
  
   titlePanel(h1("To whom children turned to when something bothered or upset them online", align = "center")),

   column(3,
          wellPanel(style = "background: #ffffff", 
            pickerInput(
              inputId = "ind_protectch",
              label = "Select metrics", 
              choices = c("Age" = "age", "Gender" = "gender", "Total" = "total"),
              options = list(
                style = "btn-primary")
          )       
   )),
   
   column(3),
   
   column(9,

wellPanel(style = "background: #ffffff", 
        plotlyOutput("protectch")
      )
   )

))




output$protectch <- renderPlotly({
  
    
    if (input$ind_protectch == "age")
    {
      fig <- plot_ly(data = data_digital(),
                     x = ~age_value, 
                     y = ~Answer, 
                     type = 'bar', 
                     color = ~age_group,
                     colors = brewer.pal(n = 3, "Paired")) %>%
        layout(title= "The last time something happened online that bothered or upset you, did you talk to anyone of these people about it?",
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
    else if (input$ind_protectch == "gender")
    {
      fig <- plot_ly(data = data_digital(),
                     x = ~gender_value, 
                     y = ~Answer, 
                     type = 'bar', 
                     color = ~gender,
                     colors = brewer.pal(n = 3, "Paired")) %>%
        layout(title= "The last time something happened online that bothered or upset you, did you talk to anyone of these people about it?",
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
        layout(title= "The last time something happened online that bothered or upset you, did you talk to anyone of these people about it?",
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
