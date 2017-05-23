
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)
source("helper.R")

# Define UI for application that draws a histogram
ui <- navbarPage("2017 Philly Primary Vote Explorer", id="nav", 
                 
      tabPanel("Interactive Map",
        div(class="outer",
            leafletOutput('voteMap', width="100%", height="100%"),
            
            absolutePanel(id="controls", class = "panel panel-default", fixed = TRUE,
                          draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                          width = 330, height = "auto",
                          
                          selectInput("office", label = h4("Office"),
                                      choices = c("District Attorney" = "DA", 
                                                  "Controller" = "Controller",
                                                  "CCP Judge (D)" = "CCP",
                                                  "Commonwealth Ct (D)" = "CWCD",
                                                  "Commonwealth Ct (R)" = "CWCR"),
                                      selected = "DA"),
                          selectInput("candidate", label = h4("Candidate"),
                                        choices = names(DApercents[,c(-1,-2,-3)]), 
                                        selected = "KRASNER..L")
            ),
            tags$div(id="cite",
                     'Created by ', tags$a(href="mailto:hollander@gmail.com", "Michael Hollander"), "Available on ", tags$a(href="asdf","GitHub")
            )
        )
      ),
      
      tabPanel("Data Explorer", 
        htmlOutput("DTTitle",container= tags$h2),
        dataTableOutput("voteTable"),
        tags$div(id="cite",
                 'Created by ', tags$a(href="mailto:hollander@gmail.com", "Michael Hollander"), "Available on ", tags$a(href="https://github.com/mhollander/2017PhillyDAPrimary","GitHub")
        )
        
      ),
      tags$head( tags$style(HTML("
          div.outer {
                                 position: fixed;
                                 top: 41px;
                                 left: 0;
                                 right: 0;
                                 bottom: 0;
                                 overflow: hidden;
                                 padding: 0;
                                 }
                                 
                                 #controls {
                                 /* Appearance */
                                 background-color: white;
                                 padding: 0 20px 20px 20px;
                                 cursor: move;
                                 /* Fade out while not hovering */
                                 opacity: 0.65;
                                 zoom: 0.9;
                                 transition: opacity 500ms 1s;
                                 }
                                 #controls:hover {
                                 /* Fade in while hovering */
                                 opacity: 0.95;
                                 transition-delay: 0;
                                 }
                                 
                                 /* Position and style citation */
                                 #cite {
                                 position: absolute;
                                 bottom: 10px;
                                 left: 10px;
                                 font-size: 14px;
                                 }           
                                 "))
      )
      
  )

server <- function(input, output, session) {
  
  session$userData$DA <- "KRASNER..L"
  session$userData$CCP <- "KRISTIANSSON..V"
  session$userData$Controller <- "RHYNHART..R"
  session$userData$CWCD <- "CEISLER..E"
  session$userData$CWCR <- "LALLEY..P"
  session$userData$Office <- "DA"
  
  output$voteMap <- renderLeaflet({
    return(daVoteMap)
  })
  
  output$DTTitle <- renderUI({
    
    HTML(paste(input$office,"Primary Results, by Ward and Division"))

  })
  
    output$voteTable <- renderDataTable({
    
      
    outputTable <- switch(input$office,
           "DA" = DApercents,
           "CCP" = CCPpercents,
           "Controller" = Contpercents,
           "CWCD" = CWDpercents,
           "CWCR" = CWRpercents)
    
    aVoteTable <- DT::datatable(outputTable[,-1],
                                options=list(
                                  pageLength = 10,
                                  lengthMenu = list(c(10, 30, 60, -1),c("10", "30", "60", 'All')),
                                  order = list(0,'asc'),
                                  searching=TRUE
                                ), 
                                class="stripe",
                                rownames=FALSE
    ) 
    return(aVoteTable)
  })
  
   observe({
     #if (is.null(session$userData$CCP) || is.na(session$userData$CCP) || session$userData$CCP=="")
    #   session$userData$CCP <- "KRISTIANSSON..V"
     
     office <- input$office
     session$userData$office <- office                     
     
     cNames <- switch(office,
                      "DA" = names(DApercents[,c(-1,-2,-3)]),
                      "CCP" = names(CCPpercents[,c(-1,-2,-3)]),
                      "Controller" = names(Contpercents[,c(-1,-2,-3)]),
                      "CWCD" = names(CWDpercents[,c(-1,-2,-3)]),
                      "CWCR" = names(CWRpercents[,c(-1,-2,-3)]))
     
     cDefault <- switch(office,
                        "DA" = session$userData$DA,
                        "CCP" = session$userData$CCP,
                        "Controller" = session$userData$Controller,
                        "CWCD" = session$userData$CWCD,
                        "CWCR" = session$userData$CWCR)
     
     updateSelectInput(session, "candidate", 
                        choices = cNames, 
                        selected = cDefault)
      
   })
   
   observe({
     if (session$userData$office == "DA") {
       
       columnData = precincts@data[[input$candidate]]
       session$userData$DA <- input$candidate
       
        # otherLayer = ifelse(input$vulnerable=="vulnerableLowStateMargin","vulnerableHighPresMargin","vulnerableLowStateMargin")
        leafletProxy("voteMap", data=precincts) %>%
          clearShapes() %>%
          addPolygons(fillColor = ~pal(columnData),
                    fillOpacity = 0.8,
                    color="#BDBDC3",
                    weight = 1,
                    popup = votePopup)
      
    }
   
   
    else if (session$userData$office == "CCP") {
     columnData = precinctsCCP@data[[input$candidate]]
     session$userData$CCP <- input$candidate

      # otherLayer = ifelse(input$vulnerable=="vulnerableLowStateMargin","vulnerableHighPresMargin","vulnerableLowStateMargin")
     leafletProxy("voteMap", data=precinctsCCP) %>%
       clearShapes() %>%
       addPolygons(fillColor = ~ccppal(columnData),
                   fillOpacity = 0.8,
                   color="#BDBDC3",
                   weight = 1,
                   popup = votePopupCCP)
    }
     
     else if (session$userData$office == "Controller") {
       columnData = precinctsCont@data[[input$candidate]]
       session$userData$Controller <- input$candidate
       
       # otherLayer = ifelse(input$vulnerable=="vulnerableLowStateMargin","vulnerableHighPresMargin","vulnerableLowStateMargin")
       leafletProxy("voteMap", data=precinctsCont) %>%
         clearShapes() %>%
         addPolygons(fillColor = ~pal(columnData),
                     fillOpacity = 0.8,
                     color="#BDBDC3",
                     weight = 1,
                     popup = votePopupCont)
     }

     else if (session$userData$office == "CWCD") {
       columnData = precinctsCWD@data[[input$candidate]]
       session$userData$CWCD <- input$candidate
       
       leafletProxy("voteMap", data=precinctsCWD) %>%
         clearShapes() %>%
         addPolygons(fillColor = ~cwdpal(columnData),
                     fillOpacity = 0.8,
                     color="#BDBDC3",
                     weight = 1,
                     popup = votePopupCWD)
     }
     else if (session$userData$office == "CWCR") {
       columnData = precinctsCWR@data[[input$candidate]]
       session$userData$CWCR <- input$candidate
       
       leafletProxy("voteMap", data=precinctsCWR) %>%
         clearShapes() %>%
         addPolygons(fillColor = ~pal(columnData),
                     fillOpacity = 0.8,
                     color="#BDBDC3",
                     weight = 1,
                     popup = votePopupCWR)
     }
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

