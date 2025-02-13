server <- function(input, output, session) {
  
  #mapping file for data dictionary input, different ones for english & spanish
  measure_mapping<-read.csv("files/measurement_dictionary.csv")
  measure_mapping_esp<-read.csv("files/measurement_dictionary_esp.csv",
                                encoding = "UTF-8")
  
  
  # Disable Step 4 on app load
  shinyjs::runjs("document.getElementById('step-2').classList.add('disabled');")
  shinyjs::disable("report")
  shinyjs::enable("downloadTemplate")
  shinyjs::runjs("document.getElementById('step-2').classList.remove('disabled');")
  
  #default language input value to english template (custom input)
  observe({
    if (is.null(input$language)) {
      updateTextInput(session, "language", value = "template.qmd")
    }
  })
  
  
  #UPDATE DICTIONARY SELECTION OPTIONS BASED ON LANGUAGE TEMPLATE
  observeEvent(input$language, {
    #update to english choices
    if(input$language == "template.qmd"){
      updated_measurements<-measure_mapping%>%
        split(.$type)%>%
        map(~ setNames(.x$file_name, .x$name))  
      
    }
    #update to spanish choices
    else if(input$language == "template_esp.qmd"){
      updated_measurements<-measure_mapping_esp%>%
        split(.$type)%>%
        map(~ setNames(.x$file_name, .x$name))  
      
    }
    
    updateVirtualSelect(
      inputId = "measurement_definitions",
      choices = updated_measurements
    )
  })
    
  
  observe({
    if(!is.null(input$format)){
      shinyjs::enable("report")
    }
    else{
      shinyjs::disable("report")
    }
  })
  
  # Page navigation
  observeEvent(input$title, {
    updateNavbarPage(session, "main_page", "page_home")
  })
  
  observeEvent(input$redirect_learn_more, {
    updateNavbarPage(session, "main_page", "page_learn_more")
  })
  
  observeEvent(input$redirect_generate_report, {
    updateNavbarPage(session, "main_page", "page_generate_report")
  })
  
  #hide tabs
  observe({
    if(input$main_page == "page_generate_report"){
    shinyjs::runjs("hideNonCurrentForms()")
    }
  })
  
  # Navigation Buttons
  observeEvent(input$next1, { shinyjs::runjs("setStep(2);") })
  observeEvent(input$prev2, { shinyjs::runjs("setStep(1);") })
  observeEvent(input$next2, { shinyjs::runjs("setStep(3);") })
  observeEvent(input$prev3, { shinyjs::runjs("setStep(2);") })
  observeEvent(input$next3, { shinyjs::runjs("setStep(4);") })
  observeEvent(input$prev4, { shinyjs::runjs("setStep(3);") })
  
  # Download report template
  output$downloadTemplate <- downloadHandler(
    filename = function() {
      paste0("soil-data-template-", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      file.copy("files/template.xlsx", file)
    }
  )
  
  # Variable to hold the uploaded data
  data <- reactiveVal(NULL)
  
  observe({
    if (!is.null(input$upload_file)) {
      # Load the required fields
      req_fields <- read_csv("files/required_fields.csv")
      
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
          ui = div(class = "alert alert-success",
                   tags$i(class = "fas fa-check"),
                   "All checks passed! No issues found.")
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
          dplyr::arrange(year) |> 
          dplyr::pull()
        
        updateSelectInput(
          session = session,
          inputId = "year",
          choices = years,
          selected = years[1] # Default to the first year
        )
        
        # Update the 'producer_id' pickerInput for the first year
        ids <- uploaded_data |> 
          dplyr::filter(year == years[1]) |> 
          dplyr::distinct(producer_id) |> 
          dplyr::arrange(producer_id) |> 
          dplyr::pull()
        
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "producer_id",
          choices = ids,
          selected = NULL # No selection by default
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
  
  # Generate reports
  rendered_reports <- reactive(paste0(Sys.Date(), "_reports.zip"))
  
  output$report <- downloadHandler(
    filename = function() { rendered_reports() },
    content = function(file) {
      shinybusy::show_modal_spinner(
        spin = 'flower',
        color = '#023B2C',
        text = 'Generating Reports...'
      )
      
      reports <- NULL
      
      withProgress(message = 'Downloading', value = 0, {
        num_producers <- length(input$producer_id)
        steps <- num_producers + 1
        
        # Generate a report for each producer
        for (i in seq_along(input$producer_id)) {
          report_name <- paste0(input$producer_id[i], ".", input$format)
          
          # Add error handling for quarto_render
          tryCatch({
            incProgress(1 / steps, detail = paste("Generating report", i, "of", num_producers))
            quarto::quarto_render(
              #input = input$language,
              input = "template.qmd",
             # output_format = "docx",
              output_format = input$format,
              execute_params = list(
                project = input$producer_id[i],
                year = input$year,
                measures = input$measurement_definitions,
                looking_forward = input$looking_forward,
                project_summary = input$project_summary
              ),
              output_file = report_name
            )
            reports <- c(reports, report_name)  
          }, error = function(e) {
            print(paste("Error during Quarto rendering for producer ID:", input$producer_id[i]))
            print(e$message)
          })
        }
        
        # Ensure reports are not empty
        if (is.null(reports) || length(reports) == 0) {
          stop("No reports were generated. Check your inputs.")
        }
        
        # Create a ZIP file containing all the reports
        incProgress(1 / steps, detail = "Zipping files...")
        tryCatch({
          zip::zip(zipfile = file, files = reports)
          print(paste("Files zipped successfully:", reports))
        }, error = function(e) {
          print("Error during zipping process:")
          print(e$message)
          stop("Failed to create ZIP file.")
        })
      })
      
      shinyjs::runjs(
      "document.getElementById('step-4').classList.add('completed');
      const step = document.getElementById('step-4');
      const circle = step.querySelector('.step-circle');
      circle.innerHTML = '<i class=\"fas fa-check\"></i>';
    ");
      
      shinybusy::remove_modal_spinner()
    }
  )
  
}
