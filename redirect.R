library(shiny)

newurl <- "http://www.google.com"

jscode <- paste0("Shiny.addCustomMessageHandler('mymessage', function(message) {window.location = '",
                  newurl, "';});")

ui <- fluidPage(
  tags$head(tags$script(jscode),
            tags$meta(`http-equiv`="refresh",
                      content=paste0("5; URL=", newurl))),
  modalDialog(tags$p("This web app changed names to", tags$em("Bird Keeper"), "and has a new location", 
                     tags$a(target = "_blank", href = newurl, newurl)),
              tags$p("If you are not to the new location within a few seconds then please click below."),
              easyClose = FALSE,
              footer = actionButton("redirectnow", "Redirect Now"))
)

server <- function(input, output, session) {
  
  observeEvent(input$redirectnow,{
    session$sendCustomMessage("mymessage", "mymessage")
    ignoreInit = TRUE
  })
}

shinyApp(ui,server)
