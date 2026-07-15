#' Read old saved file
#'
#' @description
#' Small shiny app to retrieve API tokens from previously saved data source .bin file.
#'
#' @returns
#' A Shiny app object. When run, the app allows users to upload a file and enter a password.
#' Upon clicking "Load", it attempts to decrypt the file using `safer::retrieve_object()`
#' and display a table of extracted `source` and `api` authentication details. The app will
#' halt with an error if the file is invalid or the password is incorrect.
#'
#' @export
readOldSavedFile <- function() {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::fileInput(inputId = "file", "Old File"),
      shiny::passwordInput(inputId = "pass", label = "Password"),
      shiny::actionButton("go", "Load"),
      shiny::tableOutput("out")
    ),
    server = function(input, output) {
      pass_RV <- shiny::reactiveVal()

      shiny::observe({
        pass_RV(input$pass)
      }) |>
        shiny::bindEvent(input$go)

      output$out <- shiny::renderTable({
        shiny::req(input$file, pass_RV())

        obj <- safer::retrieve_object(
          conn = input$file$datapath,
          key = pass_RV()
        )

        obj <- lapply(
          obj,
          \(x) {
            ds <- x$data_source

            if (ds == "panda") {
              api <- x$panda_auth
            } else {
              ds <- x$data_type
              api <- x$redcap_auth$token
            }

            data.frame("source" = ds, "api" = api)
          }
        ) |>
          data.table::rbindlist()

        print(obj)

        obj
      })
    }
  )
}
