server <- function(input, output, session) {
  # mapping file for data dictionary input, different ones for english & spanish
  measure_mapping <- read.csv("files/measurement_dictionary.csv") |>
    mutate(content = glue("<div>{name}</div><div style='display:none'>{aliases}</div>"))

  measure_mapping_esp <- read.csv("files/measurement_dictionary_esp.csv", encoding = "UTF-8") |>
    mutate(content = glue("<div>{name}</div><div style='display:none'>{aliases}</div>"))


  # Disable Step 4 on app load
  shinyjs::runjs("document.getElementById('step-2').classList.add('disabled');")
  shinyjs::disable("report")
  shinyjs::enable("downloadTemplate")
  shinyjs::runjs("document.getElementById('step-2').classList.remove('disabled');")

  # default language input value to english template (custom input)
  observe({
    if (is.null(input$language)) {
      updateTextInput(session, "language", value = "template.qmd")
    }
  })

  output$test <- renderUI({
    h3(input$language)
  })


  # UPDATE DICTIONARY SELECTION OPTIONS BASED ON LANGUAGE TEMPLATE
  observeEvent(input$language, {
    # update to english choices
    if (input$language == "template.qmd") {
      updated_measurements <- measure_mapping %>%
        split(.$type) %>%
        map(~ setNames(.x$file_name, .x$aliases))
    }
    # update to spanish choices
    else if (input$language == "template_esp.qmd") {
      updated_measurements <- measure_mapping_esp %>%
        split(.$type) %>%
        map(~ setNames(.x$file_name, .x$aliases))
    }

    updatePickerInput(
      inputId = "measurement_definitions",
      choices = updated_measurements,
      choicesOpt = list(content = measure_mapping$content)
    )
  })


  observe({
    if (!is.null(input$format)) {
      shinyjs::enable("report")
    } else {
      shinyjs::disable("report")
    }
  })

  # Page navigation - redirect on navbar title to home, and home buttons to section pages
  observeEvent(input$title, {
    updateNavbarPage(session, "main_page", "page_home")
  })
  observeEvent(input$redirect_learn_more, {
    updateNavbarPage(session, "main_page", "page_learn_more")
  })
  observeEvent(input$redirect_generate_report, {
    updateNavbarPage(session, "main_page", "page_generate_report")
  })

  # Hide Home Page tab in Navbar
  observe({
    if (input$main_page == "page_generate_report") {
      shinyjs::runjs("hideNonCurrentForms()")
    }
  })

  # Navigation Buttons
  observeEvent(input$next1, {
    shinyjs::runjs("setStep(2);")
  })
  observeEvent(input$prev2, {
    shinyjs::runjs("setStep(1);")
  })
  observeEvent(input$next2, {
    shinyjs::runjs("setStep(3);")
  })
  observeEvent(input$prev3, {
    shinyjs::runjs("setStep(2);")
  })
  observeEvent(input$next3, {
    shinyjs::runjs("setStep(4);")
  })
  observeEvent(input$prev4, {
    shinyjs::runjs("setStep(3);")
  })

  # Modal popup for Step 1
  observeEvent(input$aboutTemplate, {
    show_modal(
      title = "Data Template",
      id = "modal-template",
      md = "about_template"
    )
  })

  # Modal popup for Step 2
  observeEvent(input$requirementInfo, {
    show_modal(
      title = "Data Check",
      id = "modal-validation",
      md = "about_validation"
    )
  })

  # Modal popup for Step 3: Markdown
  observeEvent(input$markdown, {
    show_modal(
      title = "Markdown Basics",
      id = "modal-markdown",
      md = "about_markdown"
    )
  })

  # Modal popup for Step 3: Preview
  observeEvent(input$report_preview, {
    selected_mapping <- measure_mapping %>%
      filter(file_name %in% input$measurement_definitions)

    grouped_measures <- split(selected_mapping$file_name, selected_mapping$section_name)

    # create measure tab panels
    tabs <- lapply(names(grouped_measures), function(section_name) {
      tabPanel(
        title = section_name,
        do.call(div, lapply(grouped_measures[[section_name]], function(qmd_file) {
          includeMarkdown(read_qmd_as_md(paste0("quarto/measurements/", qmd_file)))
        }))
      )
    })

    showModal(modalDialog(
      title = "Preview Sections",
      div(
        id = "modal-preview",
        includeMarkdown("## Project Summary"),
        includeMarkdown(input$project_summary),
        includeMarkdown("## What We Measured in Your Soils"),
        tabsetPanel(id = "dynamicTabs", !!!tabs),
        includeMarkdown("## Project Results"),
        includeMarkdown(paste0(
          "All samples were collected from ",
          input$soil_depth,
          "."
        )),
        includeMarkdown("## Looking Forward"),
        includeMarkdown(input$looking_forward)
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })


  # Download report template, evaluate which template based on language selection
  output$downloadTemplate <- downloadHandler(
    filename = function() {
      template_file <- if (input$language == "template_esp.qmd") {
        "template_esp.xlsx"
      } else {
        "template.xlsx"
      }
      paste0("soil-data-template-", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      template_file <- if (input$language == "template_esp.qmd") {
        "files/template_esp.xlsx"
      } else {
        "files/template.xlsx"
      }
      file.copy(template_file, file)
    }
  )

  # Variable to hold the uploaded data
  data <- reactiveVal(NULL)

  observe({
    if (!is.null(input$upload_file)) {
      # Load the required fields
      req_fields <- read.csv("files/required_fields.csv")

      # Validate the uploaded file
      validation_results <- tryCatch(
        validate_data_file(input$upload_file$datapath, req_fields),
        error = function(e) {
          insertUI(
            selector = "#error_message",
            where = "beforeEnd",
            ui = div(class = "alert alert-danger", paste("Error during validation:", e$message))
          )
          return(NULL)
        }
      )
      if (is.null(validation_results)) {
        shinyjs::disable("report")
        shinyjs::disable("next2")
        shinyjs::runjs("document.getElementById('step-3').classList.add('disabled');")
        shinyjs::runjs("document.getElementById('step-4').classList.add('disabled');")
        return()
      }

      # Clear any existing messages
      removeUI(selector = "#error_message > *", immediate = TRUE)

      if (length(validation_results) == 0) {
        # All checks passed
        insertUI(
          selector = "#error_message",
          where = "beforeEnd",
          ui = div(
            class = "alert alert-success",
            tags$i(class = "fas fa-check"),
            "All checks passed! No issues found."
          )
        )
        shinyjs::enable("report")
        shinyjs::enable("next2")
        shinyjs::runjs("document.getElementById('step-3').classList.remove('disabled');")

        # Load the uploaded data
        uploaded_data <- readxl::read_xlsx(input$upload_file$datapath, sheet = "Data")
        data(uploaded_data) # Store data for future use

        # Update the 'year' selectInput with unique years
        years <- uploaded_data |>
          dplyr::distinct(year) |>
          dplyr::arrange(desc(year)) |>
          dplyr::pull()

        updateSelectInput(
          session = session,
          inputId = "year",
          choices = years,
          selected = years[1] # Default to the most recent year
        )

        # Update the 'producer_id' pickerInput based on the `year` selection
        observeEvent(
          input$year,
          handlerExpr = {
            ids <- uploaded_data |>
              dplyr::distinct(year, producer_id) |>
              dplyr::filter(year %in% input$year) |>
              dplyr::arrange(producer_id) |>
              dplyr::pull()

            shinyWidgets::updatePickerInput(
              session = session,
              inputId = "producer_id",
              choices = ids,
              selected = input$producer_id
            )
          }
        )
      } else {
        # Extract and display validation errors as a bulleted list
        errors <- unlist(validation_results, use.names = FALSE) # Extract only error messages
        error_ui <- div(
          class = "alert alert-danger",
          tags$strong("Validation Errors:"),
          tags$ul(
            lapply(errors, tags$li) # Create a bullet point for each error
          )
        )
        insertUI(
          selector = "#error_message",
          where = "beforeEnd",
          ui = error_ui
        )
        shinyjs::disable("report")
        shinyjs::disable("next2")
        shinyjs::runjs("document.getElementById('step-3').classList.add('disabled');")
        shinyjs::runjs("document.getElementById('step-4').classList.add('disabled');")
        shinyjs::disable("next3")
      }
    } else {
      # Reset the UI if no file is uploaded
      removeUI(selector = "#error_message > *", immediate = TRUE)
      shinyjs::disable("report")
      shinyjs::disable("next2")
      shinyjs::runjs("document.getElementById('step-3').classList.add('disabled');")
      shinyjs::runjs("document.getElementById('step-4').classList.add('disabled');")
    }
  })

  observe({
    if (is.null(input$producer_id) || length(input$producer_id) == 0) {
      # Disabl Build Repor if no producer IDs are selected
      shinyjs::runjs("document.getElementById('step-4').classList.add('disabled');")
      shinyjs::disable("report")
    } else {
      # Enable Build 4 if producer IDs are selected
      shinyjs::runjs("document.getElementById('step-4').classList.remove('disabled');")
      shinyjs::enable("report")
    }
  })

  # Create a df with inputs for quarto::quarto_render()
  quarto_input <- eventReactive(
    eventExpr = {
      input$year
      input$producer_id
    },
    valueExpr = {
      readxl::read_xlsx(input$upload_file$datapath, sheet = "Data") |>
        distinct(year, producer_id) |>
        filter(year %in% input$year & producer_id %in% input$producer_id) |>
        mutate(
          output_file = paste0(year, "_", producer_id),
          output_format = paste0(input$format, collapse = ","),
          execute_params = pmap(
            list(year, producer_id),
            ~ list(
              year = ..1,
              producer_id = ..2,
              measures = input$measurement_definitions,
              project_summary = input$project_summary,
              looking_forward = input$looking_forward
            )
          )
        ) |>
        select(output_file, output_format, execute_params) |>
        separate_longer_delim(output_format, delim = ",") |>
        mutate(
          output_file = paste0(output_file, ".", output_format)
        )
    }
  )

  # Reactive zip file name for download
  rendered_reports <- reactive(paste0(Sys.Date(), "_reports.zip"))

  # Download handler for building and zipping reports
  # Download handler for building and zipping reports
  output$report <- downloadHandler(
    filename = function() {
      rendered_reports()
    },
    content = function(file) {
      # Initiate spinner
      shinybusy::show_modal_spinner(
        spin = "flower",
        color = "#023B2C",
        text = "Building Reports..."
      )

      reports <- NULL
      report_data <- quarto_input()

      withProgress(message = "Building Reports", value = 0, {
        num_reports <- nrow(report_data)
        steps <- num_reports + 1

        # Generate a report for each producer using purrr::pwalk
        purrr::pwalk(
          report_data,
          function(output_file, output_format, execute_params) {
            tryCatch(
              {
                incProgress(1 / steps, detail = paste("Processing", output_file))

                quarto::quarto_render(
                  input = input$language,
                  output_format = output_format,
                  execute_params = execute_params,
                  output_file = output_file
                )

                reports <<- c(reports, output_file)
              },
              error = function(e) {
                warning(paste("Error during Quarto rendering for:", output_file))
                warning(e$message)
              }
            )
          }
        )

        # Make sure reports are not empty before zipping
        if (is.null(reports) || length(reports) == 0) {
          stop("No reports were generated. Check your inputs.")
        }

        # Create a ZIP file containing all  reports
        incProgress(1 / steps, detail = "Zipping files...")
        tryCatch(
          {
            zip::zip(zipfile = file, files = reports)
          },
          error = function(e) {
            warning("Error during zipping process:")
            warning(e$message)
            stop("Failed to create ZIP file.")
          }
        )
      })

      # Mark last step in stepper as complete
      shinyjs::runjs(
        "document.getElementById('step-4').classList.add('completed');
      const step = document.getElementById('step-4');
      const circle = step.querySelector('.step-circle');
      circle.innerHTML = '<i class=\"fas fa-check\"></i>';
    "
      )

      # Remove spinner
      shinybusy::remove_modal_spinner()
    }
  )
}
