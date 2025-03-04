library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinyAce)
library(shinybusy)
library(systemfonts)
library(downloadthis)
library(sever)
library(rmarkdown)
library(zip)
library(gt)
library(here)
library(tidyverse)
library(readxl)
library(glue)
library(soils)
library(webshot)
library(webshot2)
library(chromote)
library(ggplot2)
library(magick)
library(sf)
library(maptiles)
library(tidyterra)

source("utils/functions.R")
source("utils/data_validation.R")


# mapping file for english by default
measure_mapping <- read.csv("files/measurement_dictionary.csv") |>
  mutate(content = glue(
    "<div>{name}</div><span style='display:none'>{aliases}</span>"
  )) |>
  arrange(type, name)

# create measure list choices
measurement_list <- measure_mapping %>%
  split(.$type) %>%
  map(~ setNames(.x$file_name, .x$name))

measurement_content <- measure_mapping %>%
  split(.$type) %>%
  map(~ .x$content)


ui <- navbarPage(
  title = actionLink(
    inputId = "title",
    tags$div(
      style = "display:flex;gap:8px;align-items:center",
      tags$img(src = "pictures/wshi.png", style = "height:20px"),
      tags$div(class = "title-name", style = "font-size:16px", "Soil Health App")
    )
  ),
  windowTitle = "Soil Health Reports",
  id = "main_page",
  collapsible = TRUE,
  selected = "page_generate_report",
  header = tags$head(
    #add font awesome and css stylesheets
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    #add favicon
    tags$link(rel="shortcut icon", href="pictures/wshi.png"),
    #add js scripts
    tags$script(src = "scripts/toc.js"),
    tags$script(src = "scripts/stepper.js"),
    tags$script(src = "scripts/customButton.js"),
    shinyjs::useShinyjs(),
    sever::useSever(),
  ),
  tabPanel(
    "Home",
    value = "page_home",
    tags$div(
      class = "banner-bg",
      tags$div(class = "banner-overlay"),
      tags$div(
        class = "banner-content",
        h1("Soil Health Report", style = "font-size:40px"),
        p(
          style = "color:#acacac;",
          "Brought to you by the Washington State Department of Agriculture and the Washington Soil Health Initiative. Build custom soil health reports for each participant in your soil sampling project.",
          style = "font-size:20px;"
        ),
        tags$div(
          style = "display:flex;justify-content:center;gap:20px;width:100%;margin-top:20px",
          actionButton(
            "redirect_generate_report",
            "Build Reports",
            class =
              "home-btn"
          ),
          actionButton(
            "redirect_learn_more",
            "Learn More",
            class =
              "home-btn"
          )
        )
      )
    )
  ),
  tabPanel(
    title = "Build Reports",
    value = "page_generate_report",
    tags$div(
      class = "container-reports",
      tags$div(
        style = "display:flex",
        # Stepper Tree Section
        tags$div(
          class = "stepper",
          tags$div(
            class = "step active",
            id = "step-1",
            onclick = "setStep(1)",
            tags$div(
              class = "step-circle",
              html('<i class="fas fa-circle"  style="font-size:2rem"></i>')
            ),
            tags$div(
              tags$div(class = "step-num", "Step 1"),
              tags$div(class = "step-text", "Download Template")
            )
          ),
          tags$div(
            class = "step",
            id = "step-2",
            onclick = "setStep(2)",
            tags$div(class = "step-circle", shiny::icon("table")),
            tags$div(
              div(class = "step-num", "Step 2"),
              div(class = "step-text", "Upload Data")
            )
          ),
          tags$div(
            class = "step",
            id = "step-3",
            onclick = "setStep(3)",
            tags$div(class = "step-circle", shiny::icon("gear")),
            tags$div(
              tags$div(class = "step-num", "Step 3"),
              tags$div(class = "step-text", "Project Info")
            )
          ),
          tags$div(
            class = "step",
            id = "step-4",
            onclick = "setStep(4)",
            tags$div(class = "step-circle", shiny::icon("file-alt")),
            tags$div(
              tags$div(class = "step-num", "Step 4"),
              tags$div(class = "step-text", "Build Reports")
            )
          )
        ),
        # Form Section
        tags$div(
          class = "form-section",
          tags$a(
            style = "width:100%;display:flex;margin-bottom:10px;justify-content:end;",
            href = "https://github.com/WA-Department-of-Agriculture/soil-health-report-generator/issues",
            target = "_blank",
            "Report Issue"
          ),
          # progress bar for smaller screens
          tags$div(
            class = "progress-bar-container",
            # Title and Step Number
            tags$div(
              class = "progress-bar-header",
              tags$span(class = "progress-title", "Step Progress"),
              tags$span(id = "progress-step-text", class = "progress-step-text", "1/4")
            ),

            # Progress Bar
            tags$div(
              class = "progress-bar-wrapper",
              tags$div(id = "progress-bar", class = "progress-bar", style = "width: 25%;")
            )
          ),
          # Step 1 Content
          tags$div(
            class = "form-content active",
            id = "form-1",
            h4(class = "form-step", "Step 1"),
            h2(class = "form-title", "Download Template"),
            p(
              class = "form-text",
              "Choose which language the report will be in.",
              "Download the Excel template and replace the example data in the 'Data' and 'Data Dictionary' sheets with your own.",
              br(),
              br(),
              actionLink(
                inputId = "aboutTemplate",
                "Learn about the data template.",
                icon = icon("circle-info")
              )
            ),
            # tags$label("Select Report Language"),
            tags$div(
              style = "width:100%;display:flex;justify-content:center",
              customButtonInput(
                "language",
                choices = c("English" = "template.qmd", "Spanish" = "template_esp.qmd"),
                icons = c("English" = "fas fa-flag-usa", "Spanish" = "fas fa-globe"),
                multi = FALSE,
                selected = "template.qmd"
              )
            ),
            tags$div(
              style = "display:flex;justify-content:center",
              disabled(
                downloadButton(
                  "downloadTemplate",
                  "Download Template",
                  style = "margin-top:20px;width:320px;"
                )
              )
            ),
            tags$div(
              class = "buttons",
              style = "justify-content:flex-end;",
              actionButton("next1", "Next", class = "next")
            )
          ),

          # Step 2 Content
          tags$div(
            class = "form-content",
            id = "form-2",
            tags$div(
              class='form-header',
            h4(class = "form-step", "Step 2"),
            h2(class = "form-title", "Upload Data")
            ),
            tags$div(class='form-main',
                p(
                  class = "form-text",
                  "Upload your completed template to validate the data. Any checks that fail will be printed below in an error message.",
                  br(),
                  br(),
                  actionLink(
                    inputId = "requirementInfo",
                    "Learn about the data validation checks.",
                    icon = icon("circle-info")
                  )
                ),
                br(),
                fileInput(
                  "upload_file",
                  "Upload Data",
                  multiple = FALSE,
                  accept = c(
                    "text/xlsx",
                    "text/comma-separated-values,text/plain",
                    ".xlsx"
                  )
                ),
                div(id = "error_message")
            ),
            tags$div(class='form-footer',
            div(
              class = "buttons",
              actionButton("prev2", "Previous", class = "prev"),
              actionButton("next2", "Next", class = "next", disabled = TRUE)
            )
          )),

          # Step 3 Content
          tags$div(
            class = "form-content",
            id = "form-3",
            tags$div(class='form-main',
               tags$div(class='form-header',
                        h4(class = "form-step", "Step 3"),
                        h2(class = "form-title", "Project Info")
               ),
              p(
                class = "form-text",
                'Customize reports with your project-specific information and then click',
                tags$b("Preview"),
                'to see what the text will look like in the report.',
                br(),
                br(),
                actionLink(
                  inputId = "markdown",
                  label = tags$span(
                    'Learn how to use basic markdown to format text and insert links for your ', 
                    tags$b('Project Summary'),
                    'and ',
                    tags$b('Looking Forward'),
                    'sections.'),
                  icon = icon("circle-info")
                )
              ),
                  textInput(
                    inputId = "project_name",
                    label = "Project Name",
                    value = "Project Name"
                  ),
                  tags$label("Project Summary"),
                  shinyAce::aceEditor(
                    outputId = "project_summary",
                    wordWrap = TRUE,
                    mode = "markdown",
                    height = "150px",
                    value = "Thank the participating farmer. Consider including information related to how many samples you've taken, in how many crops and regions. Identify the project team and acknowledge support from your funders and collaborators.",
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "measurement_definitions",
                    label = "Measurement Definitions",
                    choices = measurement_list,
                    choicesOpt = list(content = unlist(measurement_content, recursive = FALSE)),
                    options = shinyWidgets::pickerOptions(
                      title = "Which measurements were included in your project?",
                      actionsBox = TRUE,
                      liveSearch = TRUE
                    ),
                    multiple = TRUE
                  ),
                  textInput(
                    inputId = "soil_depth",
                    label = "Soil Sample Depth",
                    value = "0-12 inches"
                  ),
                  tags$label("Looking Forward"),
                  shinyAce::aceEditor(
                    outputId = "looking_forward",
                    wordWrap = TRUE,
                    mode = "markdown",
                    height = "150px",
                    value = "Consider describing how this data will be used. Are you building decision support tools? Publications? Will you be speaking at upcoming field days or conferences about this work? Soils data can be confusing… let your audience know that this is just the start of the conversation! Thank participants once again."
                  
              ),
              actionButton(inputId = "report_preview", label = "Preview", style = "margin:20px 0px;width:100%;"),
              
            ),
            tags$div(class='form-footer',
              div(
                class = "buttons",
                actionButton("prev3", "Previous", class = "prev"),
                actionButton("next3", "Next", class = "next")
              )
            )
          ),

          # Step 4 Content
          tags$div(
            class = "form-content",
            id = "form-4",
            h4(class = "form-step", "Step 4"),
            h2(class = "form-title", "Build Reports"),
            p(
              class = "form-text",
              "Choose the year and up to five* producers to build reports for. Producer IDs listed will automatically update based on the year selected. Reports will be generated in a zip file within your Downloads folder."
            ),
            em(
              class = "form-text",
              "*During testing, only five producers are allowed at a time to prevent timeout errors. We plan to increase this limit before the public launch of the app."
            ),
            tags$div(
              class = "col-2",
              selectInput(
                inputId = "year",
                label = "Year",
                choices = NULL,
                selected = NULL,
                multiple = FALSE
              ),
              tags$div(id='producer-dropdown', style='width:100%;display:flex;justify-content:center',
                shinyWidgets::pickerInput(
                  inputId = "producer_id",
                  label = "Producer IDs",
                  choices = NULL,
                  options = shinyWidgets::pickerOptions(
                    title = "Select up to 5 producers",
                    actionsBox = TRUE,
                    liveSearch = TRUE,
                    maxOptions = 5,
                    maxOptionsText = "Maximum of 5"
                  ),
                  multiple = TRUE
                )
              )
            ),
            # Output Selection Buttons
            tags$label("Select Report Formats"),
            div(
              style = "width:100%;display:flex;justify-content:center",
              customButtonInput(
                "format",
                choices = c("Word" = "docx", "HTML" = "html"),
                icons = c("Word" = "fas fa-file-word", "HTML" = "fas fa-file-code"),
                multi = TRUE,
                selected = c("html", "docx")
              )
            ),
            div(
              class = "buttons",
              actionButton("prev4", "Previous", class = "prev"),
              disabled(downloadButton(
                "report",
                "Build",
                icon = NULL,
                class = "build"
              ))
            )
          )
        )
      )
    )
  ),
  tabPanel(
    title = "Learn More",
    value = "page_learn_more",
    create_hero("Learn More", "pictures/default-hero.png"),
    div(
      class = "content-container",
      div(
        class = "content",
        id = "content-area",
        #to modify content for Learn More please edit the markdown file
        includeMarkdown("www/content/learn_more.md")
      ),
      div(
        class = "toc-scroll",
        id = "toc-container",
        h5("FAQs")
      )
    )
  )
)
