buttonfooter3 <- function(
  lefthead,leftbody,leftfoot,
  cenhead, cenbody, cenfoot,
  righthead, rightbody, rightfoot
){
  fluidRow(class = "row-cols-1 row-cols-md-3",
           style="margin-top: 2rem; margin-bottom: 2rem;",
    navigationcard(lefthead, leftbody, leftfoot),
    navigationcard(cenhead, cenbody, cenfoot),
    navigationcard(righthead, rightbody, rightfoot)
  )
}

navigationcard <- function(head, body, foot){
    tags$div(class = "col text-center", 
     tags$div(class = "card h-100", style = "background: none; border: none;",
      tags$div(class = "card-body p-0", 
      tags$div(style = "background-color: #FFFFFF", class = "p-4 h-100",
        tags$h2(head, class="mb-4"),
        tags$div(class = "bodysmall text-center", body))),
      tags$div(class = "card-footer px-0",
	       style = "background: none; border: none;",
	       tags$div(foot, class = "my-2")))
      )
}

out1_foot <- function(){
  buttonfooter3(
    lefthead = "Edit your farm data", leftbody = "Go back to edit Scenario 1",
    leftfoot = actionButton_notdfl("out1_back", 
			            tagList(icon("angle-left", class = "me-2"), "Back"),
				   class = "btn-outline-primary py-3", width = "100%"),
    cenhead = "Download a report", cenbody = HTML(
      "Download a full report on the birds that are likely to live in your farm's woodland.",
      "This report will include a comparison between your farm and bird occupancy in an average woodland area."),
    cenfoot = downloadButton_notdfl("out1_product", "Download Report", 
                                  style = paste0("background-color: ", appcolors[["Bright Blue"]], ";"),
                                  style = "color: #FFFFFF; width: 100%;",
                                  class = "py-3" ),
    righthead = "Create a comparison", rightbody = HTML(
      "Go to the next step to create a second comparison scenario for your farm.",
      "For example, what birds might live on your farm if you increase woody vegetation cover?"),
    rightfoot = actionButton_notdfl("out1_next",
				    tagList("Next", icon("angle-right", class = "ms-2")),
				    class = "btn-primary py-3", width = "100%")
  )
}

out2_foot <- function(){
  buttonfooter3(
    lefthead = "Edit your comparison data", leftbody = "Go back to edit Scenario 2",
    leftfoot = actionButton_notdfl("out2_back", 
			            tagList(icon("angle-left", class = "me-2"), "Back"),
				   class = "btn-outline-primary py-3", width = "100%"),
    cenhead = "Download a report", cenbody = HTML("Download a full report on the birds that are likely to live in your farm's woodland.",
                                  "This report will include comparison between Scenario 1 and Scenario 2."),
    cenfoot = downloadButton_notdfl("out2_product", "Download Report",  
                                  style = paste0("background-color: ", appcolors[["Bright Blue"]], ";"),
                                  style = "color: #FFFFFF; width: 100%;",
                                  class = "py-3" ),
    righthead = "Estimation complete", rightbody = HTML(
                         "Congratulations! You have completed all the steps in", paste0(appname, "."),
                         "Visit the Sustainable Farms website for more guidance on managing natural assets to support biodiversity and productivity on farms."),
    rightfoot = 
      tags$a(href = "https://www.sustainablefarms.org.au",
	     style="width:100%;",
	     type="button",
	     class="btn btn-primary py-3",
	     "Go to Sustainable Farms")
  )
}
