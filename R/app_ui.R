#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny shinythemes
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    navbarPage("Navbar!",
               tabPanel("DataHandler", mod_DataHandler_ui("DataHandler_1")),
               tabPanel("Summary",
                        verbatimTextOutput("summary")
               ),
               navbarMenu("More",
                          tabPanel("Table",
                                   DT::dataTableOutput("table")
                          )
               )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "gapminder.app"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
