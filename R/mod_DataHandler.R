#' DataHandler UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinycssloaders withSpinner
#' @import DT 
mod_DataHandler_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Hello from the DataHandler"),
    col_12(
      withSpinner(uiOutput(ns("repo_select"), width="100%")),
      actionButton(ns("getData"), "Get data!", width="100%")
    ),
    br(),
    col_12(
      withSpinner(uiOutput(ns('repos_table')))
    ),
    col_12(
      withSpinner(verbatimTextOutput(ns("debug")))
    )
  )
}

#' DataHandler Server Functions
#'
#' @noRd 
mod_DataHandler_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    ddfObj <- reactive({
      DDFProvider$new(debug = F)
    })
    
    output$repo_select <- renderUI({
      selectizeInput(
        ns("chosenRepo"), 
        sprintf("Repositories in %s", ddfObj()$org),
        choices = ddfObj()$repos, 
        selected = ddfObj()$repos[1],
        multiple = TRUE,
        width="100%"
      )
    })
    
    output$repos_table <- renderUI({
      do.call(tabsetPanel, 
              c(id='t',
                lapply(
                  input$chosenRepo, 
                  function(repo) {
                    tabPanel(
                      title=repo, 
                      DT::renderDT({
                        ddfObj()$data[[repo]]
                      })
                    )
                  }
                )
              )
      )
    })
    
    observeEvent(input$getData, {
      updateTabsetPanel(session, "t", {
        lapply(
          input$chosenRepo, 
          function(repo) {
            tabPanel(
              title=repo, 
              DT::renderDT({
                ddfObj()$data[[repo]]
              })
            )
          }
        )
      })
    })
    
    output$debug <- renderText({ input$chosenRepo })
    
    observeEvent(input$getData, {
      print("Getting data")
      for (repo in input$chosenRepo) {
        if (is.null(ddfObj()$data[[repo]])) {
          ddfObj()$get_repo(repo)
        }
      }
      
      
      output$repos_table <<- renderUI({
        DT_EXT <- c("Responsive")
        do.call(tabsetPanel, 
                c(id='t',
                  lapply(
                    input$chosenRepo, 
                    function(repo) {
                      tabPanel(
                        title=repo, 
                        DT::renderDT({
                          ddfObj()$data[[repo]]
                        }, extensions=DT_EXT)
                      )
                    }
                  )
                )
        )
      })
    })
    
    return(ddfObj)
  })
}

## To be copied in the UI
# mod_DataHandler_ui("DataHandler_1")

## To be copied in the server
# mod_DataHandler_server("DataHandler_1")
