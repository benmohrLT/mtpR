#' R Shiny App to Design Plate Formats
#'
#' This function creates a Shiny application for designing microtiter plate formats.
#' The application allows users to set up various parameters for plate design and
#' generates a destination plate map as output.
#'
#' @export
design_plate <- function(){

  ui <- shiny::fluidPage(
    plate_designer_ui("Setup"),
  )

  server <- function(input, output, session) {

    r <- plate_designer_server("Setup",
                              createPlateMap = reactive(input$createPlateMap),
                              rxnCount = reactive(input$rxnCount),
                              replicatecount = reactive(input$replicatecount),
                              destplate = reactive(input$destplate),
                              deststartwell = reactive(input$deststartwell),
                              wellspacing = reactive(input$wellspacing),
                              replicatestyle = reactive(input$replicatestyle),
                              fillwise = reactive(input$fillwise),
                              platereplicates = reactive(input$platereplicates)
    )

  }

  shiny::shinyApp(ui, server)
}

#' Plate Design UI Module
#'
#' This UI module creates a user interface for the plate design application. It
#' provides input fields and selectors for configuring the design parameters of
#' a destination microtiter plate including type of plate, number of reactions and
#' replicates, and well spacing.
#'
#' @param id Unique identifier for UI elements to facilitate modularization.
#' @param label Label for the setup section, default is "Setup".
plate_designer_ui <- function(id, label = "Setup") {
  ns <- shiny::NS(id)

  shiny::tabPanel(
    title = "Destination Plate Setup",
    shiny::br(),
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::numericInput(
          inputId = ns("rxnCount"),
          label = "Number of reactions",
          value = 1,
          step = 1,
          min = 1
        )
      ),
      shiny::column(
        3,
        shiny::numericInput(
          inputId = ns("platereplicates"),
          label = "Number of plate replicates",
          value = 1,
          step = 1,
          min = 1
        )
      ),
      shiny::column(
        3,
        shiny::numericInput(
          inputId = ns("replicatecount"),
          label = "Number of Replicates",
          value = 3,
          min = 1
        )
      ),
    ),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::selectInput(
          inputId = ns("destplate"),
          label = "Destination Plate Type",
          choices = c("384" , "96", "48", "24", "12")
        )
      ),
      shiny::column(
        3,
        shiny::selectInput(
          inputId = ns("deststartwell"),
          label = "Starting Well",
          choices = c(
            mtpR::convert_well(
              input = 1:384,
              direction = "to_well",
              wise = "col",
              plate = 384
            )
          )
        )
      ),
      shiny::column(
        3,
        shiny::selectInput(
          inputId = ns("fillwise"),
          label = "Work by Rows or Columns?",
          choices = c(Rows = "row",
                      Columns = "col")
        )
      ),
    ),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::selectInput(
          inputId = ns("replicatestyle"),
          label = "Prioritize Reactions or Replicates",
          choices = c("Reaction", "Replicate")

        )
      ),
      shiny::column(
        3,
        shiny::numericInput(
          inputId = ns("interwell"),
          label = "Spacing across Wells",
          value = 1,
          min = 1,
          step = 1
        )
      ),
      shiny::column(
        3,
        shiny::numericInput(
          inputId = ns("intrawell"),
          label = "Spacing between Wells",
          value = 1,
          min = 1,
          step = 1
        )
      ),
    ),
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::actionButton(ns("createPlateMap"),
                   label = "Create Plate Map"),
#        shiny::br(),
#        shiny::br(),
#        shiny::radioButtons(
#          inputId = ns("toggleDestOutput"),
#          label = "Select Destination Plate Output Type",
#          choices = c("Plot", "Table")
 #       ),
    ),
    ),
    shiny::br(),
    shiny::plotOutput(outputId = ns('PlatePlot')),
    DT::dataTableOutput(outputId = ns('PlateMap'))
  )
}

#' Plate Design Server
#'
#' The server function for the plate designer Shiny module. It processes inputs
#' from the UI and updates the output of the destination plate design. It generates
#' visual or tabular representations of the designed plate.
#'
#' @param id Unique identifier for the server module.
#' @param createPlateMap Reactive expression linking the 'Create Plate Map' button.
#' @param rxnCount Reactive value for the number of reactions.
#' @param replicatecount Reactive value for the number of replicates per reaction.
#' @param destplate Reactive value for the type of destination plate.
#' @param deststartwell Reactive value for the starting well of the plate.
#' @param wellspacing Reactive value for the space between wells on the plate.
#' @param replicatestyle Reactive value for the prioritization strategy of replicates.
#' @param fillwise Reactive value indicating row-wise or column-wise filling.
#' @param platereplicates Reactive value for the number of plate replicates.
#' @return Shiny module that encapsulates the side-effects and rendering of plot and datatables based on inputs.
plate_designer_server <- function(id,
                                 createPlateMap,
                                 rxnCount,
                                 replicatecount,
                                 destplate,
                                 deststartwell,
                                 wellspacing,
                                 replicatestyle,
                                 fillwise,
                                 platereplicates
                                 ) {
  shiny::moduleServer(
    id,
    # Below is the module function
    function(input, output, session) {

      myReactives <- shiny::reactiveValues(Reactions.Dest = NULL)

      shiny::observeEvent(input$createPlateMap, {
        req(input$rxnCount)
        myReactives$Reactions.Dest <- data.frame(reaction_count = seq_len(input$rxnCount)) |>
          mtpR::replicate_reactions(reaction_col_name = "reaction_count",
                              num_replicates = input$replicatecount,
                              priority = input$replicatestyle,
                              inter_spacing = input$interwell,
                              intra_spacing = input$intrawell,
                              start_position = mtpR::convert_well(input = input$deststartwell,
                                                                  direction = "to_num",
                                                                  wise = input$fillwise,
                                                                  plate = 384)) |>
          mtpR::add_plates(well_index_column = "position",
                     plate_size = as.numeric(input$destplate)) |>
          dplyr::mutate(Well.Position = mtpR::convert_well(Well.Index,
                                                    direction = "to_well",
                                                    wise = input$fillwise,
                                                    plate = as.numeric(input$destplate)
                                                    )
                 ) |>
          mtpR::add_plate_replicates(plate_replicate_number_total = input$platereplicates) |>
          dplyr::mutate(`Plate Name` = stringr::str_glue("Plate_{Plate.Index}_{PlateReplicate.Number}"))

      })

      output$PlatePlot <- shiny::renderPlot({
        req(myReactives$Reactions.Dest)
   #     if(input$toggleDestOutput == "Plot"){
          myReactives$Reactions.Dest |>
            mtpR::plot_plate(fill = "reaction_count",
                        well_id = "Well.Position",
                        facet_rows = "Plate.Index",
                        facet_cols = "PlateReplicate.Number",
                        plate = as.numeric((input$destplate))) +
            ggplot2::labs(fill = "Reaction Number") +
            ggplot2::theme(legend.position = "bottom")
 #       }
      })

      output$PlateMap <- DT::renderDataTable({
        req(myReactives$Reactions.Dest)
   #     if(input$toggleDestOutput == "Table"){
          myReactives$Reactions.Dest |>
            DT::datatable(
              extensions = 'Buttons',
              options = list(
                dom = 'Brtip',
                buttons = c('copy', 'csv', 'excel'),
                pageLength = -1
              )
            )
   #     }
      })
      # return(myReactives$Reactions.Dest)
    }
  )
}
