
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)
source("helper.R")

# Define UI for application that draws a histogram
ui <- navbarPage("2017 Philly DA Primary Vote Explorer", id="nav", 
                 
      tabPanel("Interactive Map",
        div(class="outer",
            leafletOutput('voteMap', width="100%", height="100%"),
            
            absolutePanel(id="controls", class = "panel panel-default", fixed = TRUE,
                          draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                          width = 330, height = "auto",
                          
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
        h2("Primary Results, by Ward and Division"),
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

server <- function(input, output) {
  
  output$voteMap <- renderLeaflet({
    return(daVoteMap)
  })
  
  
  output$voteTable <- renderDataTable({
    daVoteTable <- DT::datatable(DApercents[,-1],
                                options=list(
                                  pageLength = 30,
                                  lengthMenu = list(c(15, 30, 60, -1),c("15", "30", "60", 'All')),
                                  order = list(0,'asc'),
                                  searching=TRUE
                                ), 
                                class="stripe",
                                rownames=FALSE
    ) 
    return(daVoteTable)
  })
  
   observe({
    columnData = precincts@data[[input$candidate]]
    
    # otherLayer = ifelse(input$vulnerable=="vulnerableLowStateMargin","vulnerableHighPresMargin","vulnerableLowStateMargin")
    leafletProxy("voteMap", data=precincts) %>%
      # hideGroup(otherLayer) %>%
      # showGroup(input$vulnerable)
      clearShapes() %>%
      addPolygons(fillColor = ~pal(columnData),
                  fillOpacity = 0.8,
                  color="#BDBDC3",
                  weight = 1,
                  popup = votePopup)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

