library(tidyverse)
upsetlevelch <- read_excel("data/upsetlevelch.xlsx", na = "0")

############################################### 
## Digital Skills  ----  
###############################################.    
# Create a "data_source" reactive variable
data_digital <- reactive({
  # Return the appropriate data source depending on
  # the chosen radio button
  if (input$ind_upsetlevelch == "age") {
    data_dig <- upsetlevelch %>% select("Answer", "9 - 11", "12 - 14", "15 - 17" ) %>%
      gather("age_group", "age_value", "9 - 11", "12 - 14", "15 - 17")
    
  } else if (input$ind_upsetlevelch == "gender") {
    data_dig <- upsetlevelch %>% select("Answer", Male, Female) %>%
      gather("gender", "gender_value", "Male", "Female")  
    
  }
  
  else if (input$ind_upsetlevelch == "total") {
    data_dig <- upsetlevelch %>% select("Answer", "Total")
    
  }
  return(data_dig)
})






# Already inside server
output$pageStub <- renderUI(fluidPage(

   # Application title
  
   titlePanel(h1("Children's level of being upset by exposure to harmful content online", align = "center")),

   column(3,
          wellPanel(style = "background: #ffffff", 
            pickerInput(
              inputId = "ind_upsetlevelch",
              label = "Select metrics", 
              choices = c("Age" = "age", "Gender" = "gender", "Total" = "total"),
              options = list(
                style = "btn-primary")
          )       
   )),
   
   column(3),
   
   column(9,

wellPanel(style = "background: #ffffff", 
        plotlyOutput("upsetlevelch")
      )
   )

))




output$upsetlevelch <- renderPlotly({
  
    
    if (input$ind_upsetlevelch == "age")
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
    else if (input$ind_upsetlevelch == "gender")
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
