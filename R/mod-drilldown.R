#' Drill-down module  - UI
#'
#' Patient-level data table for a selected centre, enabling quality
#' improvement workflows.
#'
#' @param id Module namespace ID.
#'
#' @return A [shiny::tagList()].
#'
#' @export
#' @family modules
mod_drilldown_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::card(
      bslib::card_header(
        shiny::span("Patient-Level Data"),
        shiny::span(
          class = "float-end",
          shiny::selectInput(
            ns("drilldown_centre"), NULL,
            choices = NULL, width = "250px"
          ),
          shiny::textInput(
            ns("drilldown_patient_id"), NULL,
            placeholder = "Patient ID", width = "150px"
          )
        )
      ),
      bslib::card_body(
        shiny::p(
          class = "text-muted",
          "Select a centre above to view individual patient data. ",
          "Click column headers to sort. Use the filter box to search."
        ),
        DT::DTOutput(ns("patient_table"))
      )
    )
  )
}

#' Drill-down module  - Server
#'
#' @param id Module namespace ID.
#' @param filtered_data Reactive tibble.
#'
#' @export
#' @family modules
mod_drilldown_server <- function(id, filtered_data) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::observe({
      centres <- sort(unique(filtered_data()$centre_code))
      shiny::updateSelectInput(session, "drilldown_centre",
                                choices = centres,
                                selected = centres[1])
    })

    drilldown_data <- shiny::reactive({
      shiny::req(input$drilldown_centre)
      df <- filtered_data() |>
        dplyr::filter(.data$centre_code == input$drilldown_centre) |>
        dplyr::select(
          `ID` = "record_id",
          `Centre` = "centre_code",
          `Consultant` = "consultant",
          `Age` = "age",
          `Gender` = "gender",
          `URR (%)` = "qblg9",
          `Pre SBP` = "qblg3",
          `Pre DBP` = "qblg4",
          `Post SBP` = "qblg6",
          `Post DBP` = "qblg7",
          `PO4` = "qblb1",
          `Ca` = "qblb4",
          `PTH` = "qblb9",
          `K+` = "qbla9",
          `HCO3` = "qbla4",
          `Hb` = "qble1",
          `Ferritin` = "qblf1",
          `Access` = "qhd20"
        )

      pid <- trimws(input$drilldown_patient_id)
      if (nchar(pid) > 0) {
        df <- df |> dplyr::filter(grepl(pid, .data$ID, ignore.case = TRUE))
      }

      df
    })

    output$patient_table <- DT::renderDT({
      hse_datatable(drilldown_data(),
                    caption = paste("Patient data  -", input$drilldown_centre),
                    pageLength = 25)
    })
  })
}
