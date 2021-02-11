devtools::load_all()
shinyApp(selectlocationUI("location"),
         function(input, output, session){
           selectlocationServer("location")
         })
