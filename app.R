library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
library(shinyglide)
library(gridExtra)
library(shinyWidgets)
library(tidyverse)
library(readxl)
library(plotly)
library(RColorBrewer)
source('./scr/data_preparation.R')

shinyApp(
  
  ui = dashboardPage(
    
    header = dashboardHeader(disable = TRUE),
    sidebar = dashboardSidebar(width = 0),
    
    body = dashboardBody(
      tags$head(
        # Note the wrapping of the string in HTML()
        tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      body {
        background-color: black;
        color: white;
      }
      h2 {
        font-family: 'Yusei Magic', sans-serif;
      }
      .shiny-input-container {
        color: #474747;
      }"))),
      ### changing theme
      shinyDashboardThemes(
        theme = "onenote"
      ),
      
      # 
      fluidRow( 
        column(12, align="center", offset = 0, style='padding:0px;',
               
               div(img(src="Picture2.jpg", height='100%', width='100%'))
        )
        
      ),
      
      # 
      br(),
      box(
        width = 12,
        title = "About",
        solidHeader = TRUE,
        header = TRUE,
        background = NULL,
        htmlOutput("about")
      ),
      box(
        width = 12,
        title = "Methodology",
        solidHeader = TRUE,
        header = TRUE,
        background = NULL,
        htmlOutput("methodology")
      ),
      fluidRow(width = 12,
          background = NULL,
        column(12, 
        glide(
          height = "800x",
          screen(
            img(src="C1.png", height='100%', width='100%')
          ),
          screen(
            img(src="C2.png", height='100%', width='100%')
          ),
          screen(
            (img(src="C3.png", height='100%', width='100%')
          ),
          screen(
            (img(src="C4.png", height='100%', width='100%')
            )
        
        
        )
      )))),
      
      fluidRow( style = "background-color:#FFFAFA00;",
                
                column(6, div(style = "font-size: 20px; padding: 0px 0px; margin-top:-2em, background-color:#FFFAFA00;"),
                       
                       prettyRadioButtons("chap", label = h3("Select study chapter"),
                                          choices = list("Access to Internet" = 1, 
                                                         "Online activities and digital skills" = 2, 
                                                         "Online risks and (potential) harm" = 3, 
                                                         "Mediation by parents, peers and teachers" = 4),
                                          icon = icon("check"), 
                                          bigger = TRUE,
                                          status = "info",
                                          animation = "smooth",
                                          outline = TRUE)
                       
                       
                       
                ),
                
                column(6, div(style = "font-size: 20px; padding: 0px 0px; margin-top:-2em, background-color:#FFFAFA00;"),
                       
                       prettyRadioButtons("cat", label = h3("Select category"),
                                          choices = list("Children" = 1, "Parents" = 2),
                                          icon = icon("check"), 
                                          bigger = TRUE,
                                          status = "info",
                                          animation = "smooth")
                       
                )),
      
      
      fluidRow( style = "background-color:#FFFAFA00;",
                
                box(
                  width = 12,
                  solidHeader = TRUE,
                  header = TRUE,
                  background = NULL,
                  ui <- uiOutput("uiStub") 
                )
           
                
      ))),
    
    
    
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    
  server = function(input, output, session) {

    observe({

      if (input$chap == "1" & input$cat == "1") {
        output$uiStub <- renderUI(tagList(             # a single-output stub ui basically lets you
          fluidPage(                                  #     move the ui into the server function
            fluidRow(
              column(12,
                     includeHTML("./html/chintaccess.html")
              )
            ),
            uiOutput("pageStub")                     # loaded server code should render the
          )                                           #    rest of the page to this output$
        ))
      }

      else if (input$chap == "1" & input$cat == "2") {
        output$uiStub <- renderUI(tagList(             # a single-output stub ui basically lets you
          fluidPage(                                  #     move the ui into the server function
            fluidRow(
              column(12,
                     includeHTML("./html/parintaccess.html")
              )
            ),
            uiOutput("pageStub")                     # loaded server code should render the
          )                                           #    rest of the page to this output$
        ))
      }
      else if (input$chap == "2" & input$cat == "1") {
        output$uiStub <- renderUI(tagList(             # a single-output stub ui basically lets you
          fluidPage(                                  #     move the ui into the server function
            fluidRow(
              column(12,
                     includeHTML("./html/onlineactchild.html")
              )
            ),
            uiOutput("pageStub")                     # loaded server code should render the
          )                                           #    rest of the page to this output$
        ))
      }
      else if (input$chap == "2" & input$cat == "2") {
        output$uiStub <- renderUI(tagList(             # a single-output stub ui basically lets you
          fluidPage(                                  #     move the ui into the server function
            fluidRow(
              column(12,
                     includeHTML("./html/onlineactpar.html")
              )
            ),
            uiOutput("pageStub")                     # loaded server code should render the
          )                                           #    rest of the page to this output$
        ))
      }
      else if (input$chap == "3" & input$cat == "1") {
        output$uiStub <- renderUI(tagList(             # a single-output stub ui basically lets you
          fluidPage(                                  #     move the ui into the server function
            fluidRow(
              column(12,
                     includeHTML("./html/onlineriskchild.html")
              )
            ),
            uiOutput("pageStub")                     # loaded server code should render the
          )                                           #    rest of the page to this output$
        ))
      }
      else if (input$chap == "3" & input$cat == "2") {
        output$uiStub <- renderUI(tagList(             # a single-output stub ui basically lets you
          fluidPage(                                  #     move the ui into the server function
            fluidRow(
              column(12,
                     includeHTML("./html/onlineriskpar.html")
              )
            ),
            uiOutput("pageStub")                     # loaded server code should render the
          )                                           #    rest of the page to this output$
        ))
      }
      else if (input$chap == "4" & input$cat == "1") {
        output$uiStub <- renderUI(tagList(             # a single-output stub ui basically lets you
          fluidPage(                                  #     move the ui into the server function
            fluidRow(
              column(12,
                     includeHTML("./html/mediachild.html")
              )
            ),
            uiOutput("pageStub")                     # loaded server code should render the
          )                                           #    rest of the page to this output$
        ))
      }
      else if (input$chap == "4" & input$cat == "2") {
        output$uiStub <- renderUI(tagList(             # a single-output stub ui basically lets you
          fluidPage(                                  #     move the ui into the server function
            fluidRow(
              column(12,
                     includeHTML("./html/mediapar.html")
              )
            ),
            uiOutput("pageStub")                     # loaded server code should render the
          )                                           #    rest of the page to this output$
        ))
      }
    })
    
    # build menu; same on all pages
    output$uiStub <- renderUI(tagList(             # a single-output stub ui basically lets you
      fluidPage(                                  #     move the ui into the server function
        fluidRow(
          column(12,
                 includeHTML("./html/aich.html")
          )
        ),
        uiOutput("pageStub")                     # loaded server code should render the
      )                                           #    rest of the page to this output$
    ))
    
    # load server code for page specified in URL
    validFiles = c("chintaccess.R", "afterconch.R", "afterconpar.R", "appwebch.R", "bullyfeelch.R",
"checkintch.R", "checkintpar.R", "childeverpar.R",  "conpar.R",  "cursourcepar.R", 
"dealuppar.R",  "desourcepar.R", "digbullych.R",  "digskillch.R", "everch.R",       
"facemeetch.R", "freqactch.R",   "freqintch.R", "friendrespondch.R", "hatesch.R",
"helpcoppar.R", "intabpar.R", "intusepar.R",  "intusfreqpar.R", "invsexch.R",    
"lasttimepar.R", "measurech.R", "measurepar.R", "meetreactch.R",  "nastytreatch.R", 
"negconch.R", "negonch.R",  "onlinexpch.R", "onlriskch.R",  "parconch.R",       
"pardigskill.R", "placesintch.R", "protectch.R", "reaslimch.R", "reaslimpar.R",
"repupsetch.R",  "restrictch.R",  "restrictpar.R",  "sexexpch.R", "sexexpmeanch.R",
"sexexpunwantch.R", "sexfeelch.R", "sexintch.R",  "sexmesch.R",  "sexmesentch.R",    
"sexplexpch.R", "sexsitch.R", "supportch.R",  "thinglastpar.R", "treatlastpar.R",    "unknowncontch.R" , 
"unwantsexintch.R", "upsetlastpar.R", "upsetlevelch.R",    "upsetpar.R", "upsetreactch.R",
"violconch.R", "weblastpar.R")                     #    for security (use all lower-case
    #    names to prevent Unix case problems)
    fname = isolate(session$clientData$url_search)       # isolate() deals with reactive context
    if(nchar(fname)==0) { fname = "?chintaccess" }              # blank means home page
    fname = paste0("./plots/", substr(fname, 2, nchar(fname)), ".R") # remove leading "?", add ".R"
    
    cat(paste0("Session filename: ", fname, ".\n"))      # print the URL for this session
    

    source(fname, local=TRUE)                            # load and run server code for this page
    
    
    
    
    
    
    
    
    
    output$about <- renderText({
      paste('<p>This interactive dashboard visualizes the findings of "One Click Away" Children&rsquo;s experience of Internet use in Albania. It presents the scientific evidence on how children use the Internet, what they are learning, the opportunities and risks they face and what parents know and don&rsquo;t know about their children&rsquo;s virtual reality. It explores the experiences of children across various dimensions of their use of the Internet and generates and sustains a rigorous cross-national comparative evidence base. In addition, the study explores the Internet use of parents and to what extent they mediate their children&rsquo;s online experiences.</p>
<p style="text-align: justify;">"One click away" was conducted in 2019 by IPSOS and UNICEF Albania and forms part of a global research project, Global Kids Online, coordinated by the UNICEF Office of Research Innocenti, the London School of Economics and Political Science (LSE) and the European Union (EU) Kids Online network. Prepared in the framework of the &lsquo;Safer and Better Internet for Children in Albania&rsquo; programme, the study was supported by the Fund to End Violence against Children.</p>
<p>The recommended citation for the publication is: Dunja A, Gjergji O, Gvineria D, Hallkaj E, and Verzivolli I. 2019 One Click Away: Children&rsquo;s Experience of Internet Use in Albania. UNICEF in Albania &amp; IPSOS Strategic Marketing, Tirana.</p>')  })

    
    
    output$methodology <- renderText({
      paste('<p>The "One Click Away" survey was conducted with a nationally representative sample of children who use the Internet of age 9 to 17 years in Albania.&nbsp; It included a total of 1,000 children from three different age groups (9&ndash;11, 12&ndash;14 and 15&ndash;17 years) and 1,000 of their parents or guardians.</p>
<p></p>
<p>Interviews were conducted in the respondents&rsquo; households, using Computer-Assisted Personal Interviewing (CAPI), in the presence of both interviewee and interviewer. The survey is in line with the ethical principles developed by UNICEF that guarantee the safety of the participants and their data and respect for their rights. In order to respect the principle of child privacy and minimize any potential negative impacts of participation in the survey, some sections in the questionnaire were self-administered.&nbsp;</p>
<p>The questionnaire was designed along the lines of the above-mentioned dimensions, grouped into five areas: access, opportunities and practices, skills, risks, and vulnerability and protection factors. The data collection tools and procedures were reviewed and approved by an External Ethical Board, commissioned by UNICEF Albania.</p>')  })
    
}

) 
