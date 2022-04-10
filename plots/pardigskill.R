library(tidyverse)
creative <- read_excel("data/creative.xlsx", na = "0")
mobile <- read_excel("data/mobile.xlsx", na = "0")
social <- read_excel("data/social.xlsx", na = "0")
information <- read_excel("data/information.xlsx", na = "0")
operational <- read_excel("data/operational.xlsx", na = "0")

############################################### 
## Digital Skills  ----  
###############################################.    
# Create a "data_source" reactive variable
data_digital <- reactive({
  # Return the appropriate data source depending on
  # the chosen radio button
  if (input$ind_pardigskill == "creative") {
    data_dig <- creative %>%  
      gather("Frequency", "Value","Not true for me", "A bit true for me", "Fairly true for me" , "Very true for me",
             "I dont know what this activity/thing means","Dont know", "Refusal" )
    
  } else if (input$ind_pardigskill == "mobile") {
    data_dig <- mobile %>% 
      gather("Frequency", "Value","Not true for me", "A bit true for me", "Fairly true for me" , "Very true for me",
             "I dont know what this activity/thing means","Dont know", "Refusal" )
    
  }
  
  else if (input$ind_pardigskill == "social") {
    data_dig <- social %>% 
      gather("Frequency", "Value","Not true for me", "A bit true for me", "Fairly true for me" , "Very true for me",
             "I dont know what this activity/thing means","Dont know", "Refusal" )
    
  }
  else if (input$ind_pardigskill == "information") {
    data_dig <- information %>% 
      gather("Frequency", "Value","Not true for me", "A bit true for me", "Fairly true for me" , "Very true for me",
             "I dont know what this activity/thing means","Dont know", "Refusal" )
    
  }
  else if (input$ind_pardigskill == "operational") {
    data_dig <- operational %>% 
      gather("Frequency", "Value","Not true for me", "A bit true for me", "Fairly true for me" , "Very true for me",
             "I dont know what this activity/thing means","Dont know", "Refusal" )
    
  }
  return(data_dig)
})






# Already inside server
output$pageStub <- renderUI(fluidPage(
  
  # Application title
  
  titlePanel(h1("Parents digital skills", align = "center")),
  
  column(3,
         wellPanel(style = "background: #ffffff", 
                   pickerInput(
                     inputId = "ind_pardigskill",
                     label = "Select digital skills type", 
                     choices = c("Creative" = "creative", "Mobile" = "mobile", "Social" = "social", "Information" = "information", "Operational" = "operational"),
                     options = list(
                       style = "btn-primary")
                   )       
         )),
  
  column(3),
  
  column(9,
         
         wellPanel(style = "background: #ffffff", 
                   plotlyOutput("pardigskill")
         )
  )
  
))




output$pardigskill <- renderPlotly({
  
  
  if (input$ind_pardigskill == "creative")
  {
    fig <- plot_ly(data = data_digital(),
                   x = ~Value, 
                   y = ~Answer, 
                   type = 'bar', 
                   orientation = 'h',
                   color = ~Frequency,
                   colors = brewer.pal(n = 3, "Paired")) %>%
      layout(title= "Parents\' confidence in digital skills",
             yaxis=list(title = "", standoff = 20L),
             xaxis=list(title = "Base: those who use internet (n=680 parents)"),
             barmode= "stack") %>% config(displaylogo = FALSE,
                                          modeBarButtonsToRemove = list(
                                            'sendDataToCloud',
                                            'autoScale2d',
                                            'resetScale2d',
                                            'hoverClosestCartesian',
                                            'hoverCompareCartesian'
                                          )) %>% layout(yaxis= list(showticklabels = FALSE))
    
    
  }         
  else if (input$ind_pardigskill == "mobile")
  {
    fig <- plot_ly(data = data_digital(),
                   x = ~Value, 
                   y = ~Answer, 
                   type = 'bar', 
                   orientation = 'h',
                   color = ~Frequency,
                   colors = brewer.pal(n = 3, "Paired")) %>%
      layout(title= "Parents\' confidence in digital skills",
             yaxis=list(title = "", standoff = 20L),
             xaxis=list(title = "Base: those who use internet (n=680 parents)"),
             barmode= "stack") %>% config(displaylogo = FALSE,
                                          modeBarButtonsToRemove = list(
                                            'sendDataToCloud',
                                            'autoScale2d',
                                            'resetScale2d',
                                            'hoverClosestCartesian',
                                            'hoverCompareCartesian'
                                          )) %>% layout(yaxis= list(showticklabels = FALSE))
  }
  else if (input$ind_pardigskill == "social")
  {
    fig <- plot_ly(data = data_digital(),
                   x = ~Value, 
                   y = ~Answer, 
                   type = 'bar', 
                   orientation = 'h',
                   color = ~Frequency,
                   colors = brewer.pal(n = 3, "Paired")) %>%
      layout(title= "Parents\' confidence in digital skills",
             yaxis=list(title = "", standoff = 20L),
             xaxis=list(title = "Base: those who use internet (n=680 parents)"),
             barmode= "stack") %>% config(displaylogo = FALSE,
                                          modeBarButtonsToRemove = list(
                                            'sendDataToCloud',
                                            'autoScale2d',
                                            'resetScale2d',
                                            'hoverClosestCartesian',
                                            'hoverCompareCartesian'
                                          )) %>% layout(yaxis= list(showticklabels = FALSE))
    
}
else if (input$ind_pardigskill == "operational")
{
  fig <- plot_ly(data = data_digital(),
                 x = ~Value, 
                 y = ~Answer, 
                 type = 'bar', 
                 orientation = 'h',
                 color = ~Frequency,
                 colors = brewer.pal(n = 3, "Paired")) %>%
  layout(title= "Parents\' confidence in digital skills",
         yaxis=list(title = "", standoff = 20L),
         xaxis=list(title = "Base: those who use internet (n=680 parents)"),
         barmode= "stack") %>% config(displaylogo = FALSE,
                                      modeBarButtonsToRemove = list(
                                        'sendDataToCloud',
                                        'autoScale2d',
                                        'resetScale2d',
                                        'hoverClosestCartesian',
                                        'hoverCompareCartesian'
                                      )) %>% layout(yaxis= list(showticklabels = FALSE))

}
else 
{
  fig <- plot_ly(data = data_digital(),
                 x = ~Value, 
                 y = ~Answer,  
                 type = 'bar', 
                 orientation = 'h',
                 color = ~Frequency,
                 colors = brewer.pal(n = 3, "Paired")) %>%
    layout(title= "Parents\' confidence in digital skills",
           yaxis=list(title = "", standoff = 20L),
           xaxis=list(title = "Base: those who use internet (n=680 parents)"),
           barmode= "stack") %>% config(displaylogo = FALSE,
                                        modeBarButtonsToRemove = list(
                                          'sendDataToCloud',
                                          'autoScale2d',
                                          'resetScale2d',
                                          'hoverClosestCartesian',
                                          'hoverCompareCartesian'
                                        )) %>% layout(yaxis= list(showticklabels = FALSE))

}
  
})
