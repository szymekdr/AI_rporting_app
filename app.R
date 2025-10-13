#!/usr/bin/env Rscript
## Simple Shiny app to pick grouped options and free-text fields
## Produces a single generated text string for copy/paste

library(shiny)
## xml2 is used for XML generation and parsing; features will be disabled if not installed
xml2_available <- requireNamespace("xml2", quietly = TRUE)

# Global taxonomy choices (used per model)
taxA_choices_glob <- list(
  "Idea generation" = "idea generation",
  "Finding new data sources" = "finding new data sources",
  "New code generation" = "new code generation",
  "Statistical model formulation" = "statistical model formulation",
  "Mathematical calculations/modelling" = "mathematical calculations/modeling",
  "Generating proposal section" = "generating proposal sections",
  "New data generation" = "new data generation",
  "Assets generation" = "assets generation",
  "Identifying appropriate methods" = "identifying appropriate methods",
  "Protocol generation/review" = "protocol generation/review",
  "Code generation for visuals" = "code generation for visuals",
  "Generative figures" = "generative figures",
  "Identification of literature to cite" = "identification of literature to cite",
  "Text generation based on prompts" = "text generation based on prompts"
)
taxB_choices_glob <- list(
  "Data cleaning" = "data cleaning",
  "Data annotation" = "data annotation",
  "Refining existing code" = "refining existing code",
  "Grant text editing and corrections" = "grant text editing and corrections",
  "Augmentation of existing data" = "augmentation of existing data",
  "Modification of existing assets" = "modification of existing assets",
  "Methods/protocols refinement" = "methods/protocols refinement",
  "Data cross-checking" = "data cross-checking",
  "Error management" = "error management",
  "Review of visual aesthetics" = "review of visual aesthetics",
  "Bibliography management" = "bibliography management",
  "Text editing for style/grammar/spelling" = "text editing for style/grammar/spelling"
)
taxC_choices_glob <- list(
  "Benchmarking against funding body guidelines" = "benchmarking against funding body guidelines",
  "Text mining of collected data" = "text mining of collected data",
  "Summarising multiple data sources" = "summarising multiple data sources",
  "Comparing multiple approaches" = "comparing multiple approaches",
  "Identifying contexts for own writing" = "identifying contexts for own writing"
)

# Custom message handler to set textarea value
js <- "Shiny.addCustomMessageHandler('setGenerated', function(message) {\n  var ta = document.getElementById('generated_text');\n  if(ta){\n    ta.value = message.value || '';\n  }\n});"

# JS handler to toggle disabled state of inputs except the 'negative' checkbox
toggle_js <- 'Shiny.addCustomMessageHandler("toggleInputs", function(message) { var disable = !!message.disable; var elems = document.querySelectorAll("input, textarea, select"); elems.forEach(function(el){ if(el.id === "negative") return; el.disabled = disable; }); });'

# JS handler to validate date format
validate_date_js <- '
$(document).on("blur", "input[id^=\'ai_date_\']", function() {
  var val = $(this).val();
  if (val && !/^\\d{4}-\\d{2}-\\d{2}$/.test(val)) {
    $(this).css("border", "2px solid red");
    alert("Date must be in YYYY-MM-DD format");
  } else {
    $(this).css("border", "");
  }
});
'

ui <- fluidPage(
  titlePanel("AI usage reporting framework: AI disclosure for Improved Transparency"),
  h1("AIdIT"),
  # Clear all inputs button
  tags$div(
    style = "margin-bottom: 10px;",
    actionButton("clear_all", "Clear all inputs", class = "btn-warning")
  ),
  # add custom JS handler for setting the generated textarea value
  tags$head(
    tags$script(HTML(js)),
    tags$script(HTML(toggle_js)),
    tags$script(HTML(validate_date_js)),
    tags$style(HTML(
      ".group-box { padding: 10px; border-radius: 6px; min-height: 120px; }
       .group-red { background-color: #ffecec; color: #8b0000; }
       .group-green { background-color: #ecffed; color: #006400; }
       .group-blue { background-color: #ecfdff; color: #0a0a91; }
       .group-red .checkbox, .group-red .checkbox-inline { color: #8b0000; }
       .group-green .checkbox, .group-green .checkbox-inline { color: #006400; }
       .group-blue .checkbox, .group-blue .checkbox-inline { color: #0a0a91; }
       .group-title { font-weight: 600; margin-bottom: 6px; }
       @media (max-width: 768px) { .group-box { min-height: auto; margin-bottom: 10px; } }
      "
    ))
  ),
  # Use a fluidRow so we can make the left column narrower and the right column wider
  fluidRow(
    # Left: generated text (narrow)
    column(
      width = 4,
      wellPanel(
        h4("Generated text (read-only)"),
        tags$textarea(id = "generated_text", rows = 10, style = "width:100%;", readonly = NA),
        br(),
        actionButton("copy_btn", "Copy to clipboard"),
        br(),
        br(),
        downloadButton("download_xml", "Download XML", class = "btn-secondary"),
        br(),
        br(),
        fileInput("upload_xml", "Load from XML", accept = c(".xml")),
        {
          xml_msg <- if (xml2_available) "XML import/export available." else "Install the xml2 package to enable XML import/export."
          tags$small(xml_msg)
        }
      )
    ),

    # Right: options (wider)
    column(
      width = 8,
      h4("Negative AI statement (no AI used)"),
      checkboxInput("negative",
        "I have not used any AI tools in this work",
        value = FALSE
      ),
      hr(),
      h4("Listing of AI engines used in the paper:"),
      # let user choose how many models to enter (default 1)
      numericInput("num_models", "No. of AI models used",
        value = 1, min = 1, max = 10, step = 1, width = "150px"
      ),
      tags$div(
        class = "ai-table",
        # header row
        fluidRow(
          column(3, tags$strong("")),
          column(3, tags$strong("Model name")),
          column(3, tags$strong("Model version")),
          column(3, tags$strong("Usage date (YYYY-MM-DD)"))
        ),
        # dynamic rows generated server-side
        uiOutput("ai_rows")
      ),
      # hr(),
      # h4("Taxonomy of AI uses (tick all that apply)"),
      # h5("AI was used in content ..."),
      # fluidRow(
      #   column(
      #     4,
      #     div(
      #       class = "group-box group-red",
      #       tags$div(class = "group-title", "... generation"),
      #       h6("Conceptualisation"),
      #       checkboxGroupInput("group1",
      #         label = NULL,
      #         choices = list("Idea generation" = "idea generation")
      #       ),
      #       h6("Data curation"),
      #       checkboxGroupInput("group2",
      #         label = NULL,
      #         choices = list("Finding new data sources" = "finding new data sources")
      #       ),
      #       h6("Formal analysis"),
      #       checkboxGroupInput("group3",
      #         label = NULL,
      #         choices = list(
      #           "New code generation" = "new code generation",
      #           "Statistical model formulation" = "statistical model formulation",
      #           "Mathematical calculations/modelling" = "mathematical calculations/modeling"
      #         )
      #       ),
      #       h6("Funding acquisition"),
      #       checkboxGroupInput("group4",
      #         label = NULL,
      #         choices = list("Generating proposal sections" = "generating proposal sections")
      #       ),
      #       h6("Investigation"),
      #       checkboxGroupInput("group5",
      #         label = NULL,
      #         choices = list(
      #           "New data generation" = "new data generation",
      #           "Assets generation" = "assets generation"
      #         )
      #       ),
      #     )
      #   ),
      #   column(
      #     4,
      #     div(
      #       class = "group-box group-green",
      #       tags$div(class = "group-title", "... refining"),
      #       checkboxGroupInput("group2",
      #         label = NULL,
      #         choices = list(
      #           "option4" = "option4",
      #           "option5" = "option5",
      #           "option6" = "option6"
      #         )
      #       )
      #     )
      #   ),
      #   column(
      #     4,
      #     div(
      #       class = "group-box group-blue",
      #       tags$div(class = "group-title", "... comparisons"),
      #       checkboxGroupInput("group3",
      #         label = NULL,
      #         choices = list(
      #           "option4" = "option4",
      #           "option5" = "option5",
      #           "option6" = "option6"
      #         )
      #       )
      #     )
      #   )
      # ),
      br(),
      actionButton("generate", "Generate text", class = "btn-primary")
    )
  )
)

server <- function(input, output, session) {
  generated <- reactiveVal("")
  
  # Reactive value to hold XML data waiting to be applied
  pending_xml_load <- reactiveVal(NULL)

  make_text <- function() {
    # If the user ticks the negative checkbox, return the fixed negative statement
    if (!is.null(input$negative) && isTRUE(input$negative)) {
      return("No AI tools were intentionally used in this study or in writing this paper.")
    }

    # Gather all selected options from all groups
    # opts <- c(input$group1, input$group2, input$group3)
    # opts <- opts[!is.null(opts) & nzchar(opts)]
    # opts_str <- if (length(opts) == 0) "none" else paste(opts, collapse = "; ")

    # Gather AI model rows (dynamic number specified by num_models)
    n_models <- if (!is.null(input$num_models) && input$num_models >= 1) as.integer(input$num_models) else 1L
    rows <- lapply(seq_len(n_models), function(i) {
      name <- input[[paste0("ai_name_", i)]]
      ver <- input[[paste0("ai_version_", i)]]
      date <- input[[paste0("ai_date_", i)]]
      
      # Validate date format if provided
      if (!is.null(date) && nzchar(trimws(date))) {
        if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", date)) {
          stop(paste0("Invalid date format for model ", i, ". Please use YYYY-MM-DD format."))
        }
      }
      
      # consider row empty if name is empty
      if (is.null(name) || !nzchar(trimws(name))) {
        return(NULL)
      }
      # collect taxonomy uses for this model from all fragmented inputs
      uses <- character(0)
      # Collect from taxA1-taxA8
      for (j in 1:8) {
        vals <- input[[paste0("taxA", j, "_", i)]]
        if (!is.null(vals) && length(vals) > 0) uses <- c(uses, vals)
      }
      # Collect from taxB1-taxB8
      for (j in 1:8) {
        vals <- input[[paste0("taxB", j, "_", i)]]
        if (!is.null(vals) && length(vals) > 0) uses <- c(uses, vals)
      }
      # Collect from taxC1-taxC5
      for (j in 1:5) {
        vals <- input[[paste0("taxC", j, "_", i)]]
        if (!is.null(vals) && length(vals) > 0) uses <- c(uses, vals)
      }
      uses <- unique(uses)

      # build a compact representation: name (version/date info)
      meta_parts <- c()
      if (!is.null(ver) && nzchar(trimws(ver))) meta_parts <- c(meta_parts, paste0("v. ", trimws(ver)))
      if (!is.null(date) && nzchar(trimws(date))) meta_parts <- c(meta_parts, paste0("timestamp: ", trimws(date)))
      meta <- if (length(meta_parts) > 0) paste0(" (", paste(meta_parts, collapse = "; "), ")") else ""
      list(name = trimws(name), meta = paste0(trimws(name), meta), uses = uses)
    })
    rows <- Filter(function(x) !is.null(x) && nzchar(x$name), rows)

  # global opts (kept for backward compatibility) -- collect all group selections
  opts <- c(input$group1, input$group2, input$group3, input$group4, input$group5)
  if (is.null(opts)) opts <- character(0)
  # remove empty strings / NA
  opts <- opts[!is.na(opts) & nzchar(opts)]
  opts_str <- if (length(opts) == 0) "none" else paste(opts, collapse = "; ")

    # per-model usage formatting: "ModelName - use1, use2; Model2 - ..."
    per_model_usage <- vapply(rows, function(r) {
      if (is.null(r$uses) || length(r$uses) == 0) {
        return(NA_character_)
      }
      paste0(r$name, " - ", paste(r$uses, collapse = ", "))
    }, FUN.VALUE = "")
    per_model_usage <- per_model_usage[!is.na(per_model_usage) & nzchar(per_model_usage)]
    per_model_usage_str <- if (length(per_model_usage) == 0) "" else paste(per_model_usage, collapse = "; ")

    model_metas <- vapply(rows, function(r) r$meta, FUN.VALUE = "")

    # final composed text
    if (nzchar(per_model_usage_str)) {
      paste0("We used the following AI engines in the present study: ", paste(model_metas, collapse = "; "), ". Area(s) of generative AI usage: ", per_model_usage_str, ".")
    } else {
      paste0("We used the following AI engines in the present study: ", paste(model_metas, collapse = "; "), ". Area(s) of generative AI usage: ", opts_str, ".")
    }
  }

  observeEvent(input$generate, {
    txt <- tryCatch({
      make_text()
    }, error = function(e) {
      showNotification(e$message, type = "error", duration = 5)
      return(NULL)
    })
    
    if (!is.null(txt)) {
      generated(txt)
      # set textarea content via JS so it's visible in the readonly tag
      session$sendCustomMessage(type = "setGenerated", message = list(value = txt))
    }
  })

  # render dynamic rows of AI inputs
  output$ai_rows <- renderUI({
    n_models <- if (!is.null(input$num_models) && input$num_models >= 1) as.integer(input$num_models) else 1L
    n_models <- min(max(n_models, 1L), 20L)
    rows_ui <- lapply(seq_len(n_models), function(i) {
      # Preserve existing values when re-rendering
      current_name <- isolate(input[[paste0("ai_name_", i)]])
      current_version <- isolate(input[[paste0("ai_version_", i)]])
      current_date <- isolate(input[[paste0("ai_date_", i)]])
      
      tagList(
        fluidRow(
          column(3, tags$div(style = "margin-top:6px;", tags$em("Enter details of AI model used:"))),
          column(3, textInput(paste0("ai_name_", i), label = NULL, value = if(is.null(current_name)) "" else current_name)),
          column(3, textInput(paste0("ai_version_", i), label = NULL, value = if(is.null(current_version)) "" else current_version)),
          column(3, textInput(paste0("ai_date_", i), label = NULL, value = if(is.null(current_date)) "" else current_date, placeholder = "YYYY-MM-DD"))
        ),
        # taxonomy for this model: three groups with section labels
        h5("This AI engine was used in content ..."),
        fluidRow(
          column(
            4,
            div(
              class = "group-box group-red",
              tags$div(class = "group-title", "... generation"),
              h6("Conceptualisation"),
              checkboxGroupInput(paste0("taxA1_", i), label = NULL, choices = taxA_choices_glob[1], selected = isolate(input[[paste0("taxA1_", i)]])),
              h6("Data curation"),
              checkboxGroupInput(paste0("taxA2_", i), label = NULL, choices = taxA_choices_glob[2], selected = isolate(input[[paste0("taxA2_", i)]])),
              h6("Formal analysis"),
              checkboxGroupInput(paste0("taxA3_", i), label = NULL, choices = taxA_choices_glob[3:5], selected = isolate(input[[paste0("taxA3_", i)]])),
              h6("Funding acquisition"),
              checkboxGroupInput(paste0("taxA4_", i), label = NULL, choices = taxA_choices_glob[6], selected = isolate(input[[paste0("taxA4_", i)]])),
              h6("Investigation"),
              checkboxGroupInput(paste0("taxA5_", i), label = NULL, choices = taxA_choices_glob[7:8], selected = isolate(input[[paste0("taxA5_", i)]])),
              h6("Methods"),
              checkboxGroupInput(paste0("taxA6_", i), label = NULL, choices = taxA_choices_glob[9:10], selected = isolate(input[[paste0("taxA6_", i)]])),
              h6("Visualisation"),
              checkboxGroupInput(paste0("taxA7_", i), label = NULL, choices = taxA_choices_glob[11:12], selected = isolate(input[[paste0("taxA7_", i)]])),
              h6("Writing"),
              checkboxGroupInput(paste0("taxA8_", i), label = NULL, choices = taxA_choices_glob[13:14], selected = isolate(input[[paste0("taxA8_", i)]]))
            )
          ),
          column(
            4,
            div(
              class = "group-box group-green",
              tags$div(class = "group-title", "... refining"),
              h6("Data curation"),
              checkboxGroupInput(paste0("taxB1_", i), label = NULL, choices = taxB_choices_glob[1:2], selected = isolate(input[[paste0("taxB1_", i)]])),
              h6("Formal analysis"),
              checkboxGroupInput(paste0("taxB2_", i), label = NULL, choices = taxB_choices_glob[3], selected = isolate(input[[paste0("taxB2_", i)]])),
              h6("Funding acquisition"),
              checkboxGroupInput(paste0("taxB3_", i), label = NULL, choices = taxB_choices_glob[4], selected = isolate(input[[paste0("taxB3_", i)]])),
              h6("Investigation"),
              checkboxGroupInput(paste0("taxB4_", i), label = NULL, choices = taxB_choices_glob[5:6], selected = isolate(input[[paste0("taxB4_", i)]])),
              h6("Methods"),
              checkboxGroupInput(paste0("taxB5_", i), label = NULL, choices = taxB_choices_glob[7], selected = isolate(input[[paste0("taxB5_", i)]])),
              h6("Validation"),
              checkboxGroupInput(paste0("taxB6_", i), label = NULL, choices = taxB_choices_glob[8:9], selected = isolate(input[[paste0("taxB6_", i)]])),
              h6("Visualisation"),
              checkboxGroupInput(paste0("taxB7_", i), label = NULL, choices = taxB_choices_glob[10], selected = isolate(input[[paste0("taxB7_", i)]])),
              h6("Writing"),
              checkboxGroupInput(paste0("taxB8_", i), label = NULL, choices = taxB_choices_glob[11:12], selected = isolate(input[[paste0("taxB8_", i)]]))
            )
          ),
          column(
            4,
            div(
              class = "group-box group-blue",
              tags$div(class = "group-title", "... comparisons"),
              h6("Conceptualisation"),
              checkboxGroupInput(paste0("taxC1_", i), label = NULL, choices = taxC_choices_glob[1], selected = isolate(input[[paste0("taxC1_", i)]])),
              h6("Data curation"),
              checkboxGroupInput(paste0("taxC2_", i), label = NULL, choices = taxC_choices_glob[2], selected = isolate(input[[paste0("taxC2_", i)]])),
              h6("Formal analysis"),
              checkboxGroupInput(paste0("taxC3_", i), label = NULL, choices = taxC_choices_glob[3], selected = isolate(input[[paste0("taxC3_", i)]])),
              h6("Funding acquisition"),
              checkboxGroupInput(paste0("taxC4_", i), label = NULL, choices = taxC_choices_glob[4], selected = isolate(input[[paste0("taxC4_", i)]])),
              h6("Writing"),
              checkboxGroupInput(paste0("taxC5_", i), label = NULL, choices = taxC_choices_glob[5], selected = isolate(input[[paste0("taxC5_", i)]]))
            )
          )
        ),
        tags$hr()
      )
    })
    do.call(tagList, rows_ui)
  })
  
  # Watch for when UI is rendered and apply pending XML data
  observe({
    xml_data <- pending_xml_load()
    if (is.null(xml_data)) return()
    
    # Check if the UI inputs exist
    n_models <- length(xml_data$models)
    if (n_models > 0) {
      # Check if first and last inputs exist
      first_exists <- !is.null(input[[paste0("ai_name_1")]])
      last_exists <- !is.null(input[[paste0("ai_name_", n_models)]])
      
      if (!first_exists || !last_exists) {
        return()  # UI not ready yet
      }
      
      # Apply the XML data
      isolate({
        i <- 1L
        for (model_data in xml_data$models) {
          updateTextInput(session, paste0("ai_name_", i), value = model_data$name)
          updateTextInput(session, paste0("ai_version_", i), value = model_data$version)
          updateTextInput(session, paste0("ai_date_", i), value = model_data$date)
          
          # Apply uses
          if (length(model_data$uses) > 0) {
            for (use_val in model_data$uses) {
              # Check which taxonomy group it belongs to
              if (use_val %in% unlist(taxA_choices_glob)) {
                for (j in 1:8) {
                  input_id <- paste0("taxA", j, "_", i)
                  # Check if this specific checkbox group contains this choice
                  choices_subset <- taxA_choices_glob[if(j==1) 1 else if(j==2) 2 else if(j==3) 3:5 else if(j==4) 6 else if(j==5) 7:8 else if(j==6) 9:10 else if(j==7) 11:12 else 13:14]
                  if (use_val %in% unlist(choices_subset)) {
                    current <- input[[input_id]]
                    if (is.null(current)) current <- character(0)
                    updateCheckboxGroupInput(session, input_id, selected = unique(c(current, use_val)))
                  }
                }
              } else if (use_val %in% unlist(taxB_choices_glob)) {
                for (j in 1:8) {
                  input_id <- paste0("taxB", j, "_", i)
                  choices_subset <- taxB_choices_glob[if(j==1) 1:2 else if(j==2) 3 else if(j==3) 4 else if(j==4) 5:6 else if(j==5) 7 else if(j==6) 8:9 else if(j==7) 10 else 11:12]
                  if (use_val %in% unlist(choices_subset)) {
                    current <- input[[input_id]]
                    if (is.null(current)) current <- character(0)
                    updateCheckboxGroupInput(session, input_id, selected = unique(c(current, use_val)))
                  }
                }
              } else if (use_val %in% unlist(taxC_choices_glob)) {
                for (j in 1:5) {
                  input_id <- paste0("taxC", j, "_", i)
                  choices_subset <- taxC_choices_glob[j]
                  if (use_val %in% unlist(choices_subset)) {
                    current <- input[[input_id]]
                    if (is.null(current)) current <- character(0)
                    updateCheckboxGroupInput(session, input_id, selected = unique(c(current, use_val)))
                  }
                }
              }
            }
          }
          i <- i + 1L
        }
        
        # Apply groups
        for (g in names(xml_data$groups)) {
          updateCheckboxGroupInput(session, g, selected = xml_data$groups[[g]])
        }
        
        # Clear pending data
        pending_xml_load(NULL)
        showNotification("XML imported, form updated", type = "message")
      })
    }
  })

  # initialize empty value on load
  observe({
    session$sendCustomMessage(type = "setGenerated", message = list(value = ""))
    # set inputs enabled/disabled according to initial negative value
    session$sendCustomMessage(type = "toggleInputs", message = list(disable = isTRUE(input$negative)))
  })

  # toggle all inputs when negative checkbox is changed
  observeEvent(input$negative, {
    session$sendCustomMessage(type = "toggleInputs", message = list(disable = isTRUE(input$negative)))
  })

  # clear all inputs when button pressed
  observeEvent(input$clear_all, {
    # reset numeric input
    updateNumericInput(session, "num_models", value = 1)
    # reset negative checkbox
    updateCheckboxInput(session, "negative", value = FALSE)
    # reset checkbox groups
    updateCheckboxGroupInput(session, "group1", selected = character(0))
    updateCheckboxGroupInput(session, "group2", selected = character(0))
    updateCheckboxGroupInput(session, "group3", selected = character(0))
    # clear ai fields up to 20
    for (i in 1:20) {
      updateTextInput(session, paste0("ai_name_", i), value = "")
      updateTextInput(session, paste0("ai_version_", i), value = "")
      updateTextInput(session, paste0("ai_date_", i), value = "")
      # clear taxonomy selections for each potential model (fragmented IDs)
      for (j in 1:8) {
        updateCheckboxGroupInput(session, paste0("taxA", j, "_", i), selected = character(0))
        updateCheckboxGroupInput(session, paste0("taxB", j, "_", i), selected = character(0))
      }
      for (j in 1:5) {
        updateCheckboxGroupInput(session, paste0("taxC", j, "_", i), selected = character(0))
      }
    }
    # clear generated text
    session$sendCustomMessage(type = "setGenerated", message = list(value = ""))
    # ensure inputs enabled
    session$sendCustomMessage(type = "toggleInputs", message = list(disable = FALSE))
  })

  # Download XML representing current app state
  output$download_xml <- downloadHandler(
    filename = function() {
      paste0("ai_report_state_", Sys.Date(), ".xml")
    },
    content = function(file) {
      if (!xml2_available) {
        showNotification("xml2 package not available; cannot export XML", type = "error")
        return()
      }
      # build XML
      doc <- xml2::xml_new_root("AIReport")
      xml2::xml_set_attr(doc, "generated_at", as.character(Sys.time()))
      # negative
      xml2::xml_add_child(doc, "negative", as.character(isTRUE(input$negative)))
      # num models
      xml2::xml_add_child(doc, "num_models", as.character(input$num_models))
      # ai models
      models_node <- xml2::xml_add_child(doc, "models")
      n_models <- if (!is.null(input$num_models)) as.integer(input$num_models) else 0L
      for (i in seq_len(max(n_models, 0))) {
        name <- input[[paste0("ai_name_", i)]]
        ver <- input[[paste0("ai_version_", i)]]
        date <- input[[paste0("ai_date_", i)]]
        row_node <- xml2::xml_add_child(models_node, "model")
        xml2::xml_add_child(row_node, "name", ifelse(is.null(name), "", name))
        xml2::xml_add_child(row_node, "version", ifelse(is.null(ver), "", ver))
        xml2::xml_add_child(row_node, "date", ifelse(is.null(date), "", date))
        # per-model uses (collect from fragmented taxA/taxB/taxC inputs)
        uses_node <- xml2::xml_add_child(row_node, "uses")
        # taxA1-taxA8
        for (j in 1:8) {
          vals <- input[[paste0("taxA", j, "_", i)]]
          if (!is.null(vals) && length(vals) > 0) {
            for (v in vals) xml2::xml_add_child(uses_node, "use", v)
          }
        }
        # taxB1-taxB8
        for (j in 1:8) {
          vals <- input[[paste0("taxB", j, "_", i)]]
          if (!is.null(vals) && length(vals) > 0) {
            for (v in vals) xml2::xml_add_child(uses_node, "use", v)
          }
        }
        # taxC1-taxC5
        for (j in 1:5) {
          vals <- input[[paste0("taxC", j, "_", i)]]
          if (!is.null(vals) && length(vals) > 0) {
            for (v in vals) xml2::xml_add_child(uses_node, "use", v)
          }
        }
      }
      # groups
      groups_node <- xml2::xml_add_child(doc, "groups")
      for (g in c("group1", "group2", "group3", "group4", "group5")) {
        vals <- input[[g]]
        gnode <- xml2::xml_add_child(groups_node, g)
        if (!is.null(vals) && length(vals) > 0) {
          for (v in vals) xml2::xml_add_child(gnode, "val", v)
        }
      }
      # write to file
      xml2::write_xml(doc, file)
    }
  )

  # Upload XML and restore state
  observeEvent(input$upload_xml, {
    if (is.null(input$upload_xml)) {
      return()
    }
    if (!xml2_available) {
      showNotification("xml2 package not available; cannot import XML", type = "error")
      return()
    }
    path <- input$upload_xml$datapath
    doc <- tryCatch(xml2::read_xml(path), error = function(e) NULL)
    if (is.null(doc)) {
      showNotification("Failed to parse XML", type = "error")
      return()
    }
    
    # Parse and store all XML data
    xml_data <- list()
    
    # negative
    neg <- xml2::xml_text(xml2::xml_find_first(doc, ".//negative"))
    xml_data$negative <- identical(tolower(neg), "true") || identical(neg, "1")
    
    # num_models
    nm <- xml2::xml_text(xml2::xml_find_first(doc, ".//num_models"))
    if (nzchar(nm) && !is.na(as.integer(nm))) {
      xml_data$num_models <- as.integer(nm)
    } else {
      xml_data$num_models <- 1
    }
    
    # models
    model_nodes <- xml2::xml_find_all(doc, ".//models/model")
    xml_data$models <- list()
    i <- 1L
    for (mn in model_nodes) {
      model_data <- list()
      model_data$name <- xml2::xml_text(xml2::xml_find_first(mn, "./name"))
      model_data$version <- xml2::xml_text(xml2::xml_find_first(mn, "./version"))
      model_data$date <- xml2::xml_text(xml2::xml_find_first(mn, "./date"))
      
      # restore per-model uses
      uses <- xml2::xml_find_all(mn, "./uses/use")
      if (length(uses) > 0) {
        model_data$uses <- vapply(uses, xml2::xml_text, "")
      } else {
        model_data$uses <- character(0)
      }
      xml_data$models[[i]] <- model_data
      i <- i + 1L
    }
    
    # groups
    xml_data$groups <- list()
    for (g in c("group1", "group2", "group3", "group4", "group5")) {
      vals <- xml2::xml_find_all(doc, paste0(".//groups/", g, "/val"))
      if (length(vals) > 0) {
        xml_data$groups[[g]] <- vapply(vals, xml2::xml_text, "")
      } else {
        xml_data$groups[[g]] <- character(0)
      }
    }
    
    # Update negative checkbox immediately
    updateCheckboxInput(session, "negative", value = xml_data$negative)
    
    # Update num_models (this will trigger UI re-render)
    updateNumericInput(session, "num_models", value = xml_data$num_models)
    
    # Store the data - the observe() watching pending_xml_load will apply it when UI is ready
    pending_xml_load(xml_data)
  })
}

app <- shinyApp(ui = ui, server = server)

## Run the app when in interactive mode OR when called via Rscript
should_run <- interactive() || grepl("Rscript", paste(commandArgs(), collapse = " "))
if (should_run) {
  runApp(app)
}

# When this file is sourced by a hosting service it should return the
# shiny.appobj; having `app` as the final expression ensures that.
app
