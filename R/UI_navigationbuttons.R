buttonfooter3 <- function(
  lefthead,leftbody,leftfoot,
  cenhead, cenbody, cenfoot,
  righthead, rightbody, rightfoot
){
  fluidRow(class = "align-items-end",
           style="margin-top: 2rem; margin-bottom: 2rem;",
    column(4, class = "text-center",
      tags$div(style = "background-color: #FFFFFF", class = "p-4",
        tags$h2(lefthead, class="mb-4"),
        tags$div(class = "bodysmall text-center", leftbody)),
      tags$div(leftfoot, class = "my-2")
      ),
    column(4, class = "text-center",
           tags$div(style = "background-color: #FFFFFF", class = "p-4",
                    tags$h2(cenhead, class="mb-4"),
                    tags$div(class = "bodysmall text-center", cenbody)),
           tags$div(cenfoot, class = "my-2")
    ),
    column(4, class = "text-center",
           tags$div(style = "background-color: #FFFFFF", class = "p-4",
                    tags$h2(righthead, class="mb-4"),
                    tags$div(class = "bodysmall text-center", rightbody)),
           tags$div(rightfoot, class = "my-2")
    )
  )
}

out1_foot <- function(){
  buttonfooter3(
    lefthead = "Edit your farm data", leftbody = "Go back to edit Scenario 1",
    leftfoot = actionButton_notdfl("out1_back", "Back", class = "btn-outline-primary py-3", width = "100%"),
    cenhead = "Download the full report", cenbody = HTML(
      "Download a full report on the birds that are likely to live in your farm's woodland.",
      "This report will include a comparison between your farm and bird occupancy in an average woodland area."),
    cenfoot = downloadButton("out1_product", "Download Report", 
                                  style = paste0("background-color: ", appcolors[["Bright Blue"]], ";"),
                                  style = "color: #FFFFFF; width: 100%;",
                                  class = "py-3" ),
    righthead = "Create a Comparison", rightbody = HTML(
      "Go to the next step to create a second comparison scenario for your farm.",
      "For example, what birds might live on your farm if you increase woody vegetation cover?"),
    rightfoot = actionButton_notdfl("out1_next", "Next", class = "btn-primary py-3", width = "100%")
  )
}

out2_foot <- function(){
  buttonfooter3(
    lefthead = "Edit your comparison data", leftbody = "Go back to edit Scenario 2",
    leftfoot = actionButton_notdfl("out2_back", "Back", class = "btn-outline-primary py-3", width = "100%"),
    cenhead = "Download the full report", cenbody = HTML("Download a full report on the birds that are likely to live in your farm's woodland.",
                                  "This report will include comparison between Scenario 1 and Scenario 2."),
    cenfoot = downloadButton("out2_product", "Download Report",  
                                  style = paste0("background-color: ", appcolors[["Bright Blue"]], ";"),
                                  style = "color: #FFFFFF; width: 100%;",
                                  class = "py-3" ),
    righthead = "Estimation Complete", rightbody = HTML(
                         "Congratulations! You have completed all the steps in", paste0(appname, "."),
                         "Visit the Sustainable Farms website for more guidance on supporting sustainable and profitable agriculture."),
    rightfoot = 
      tags$a(href = "https://www.sustainablefarms.org.au",
	     style="width:100%;",
	     type="button",
	     class="btn btn-primary py-3",
	     "Go to Sustainable Farms")
  )
}
