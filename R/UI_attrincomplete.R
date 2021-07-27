patchincompletewarn <- 
	tags$span(class="glyphicon glyphicon-warning-sign", 
		  style = "color: #CC9900;",
		  tabindex = "0",
		  `data-toggle` = "tooltip",
		  title = "Please set the attributes of this patch.",
		  `data-trigger` = "focus hover",
		  `data-placement` = "auto"
		  )
patchcompletesymbol <- 
  tags$span(class="glyphicon glyphicon-ok", 
            style = "color: white;")

