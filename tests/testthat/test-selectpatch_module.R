devtools::load_all()
shinyApp(selectpatchUI("patch"),
         function(input, output, session){
           selectpatchServer("patch")
         })
