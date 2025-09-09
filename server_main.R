# server_main.R
# This module contains the logic for the "Main Analysis" tab.
library(grid)
library(gridGraphics)

# =========================================================================
# UTILITY FUNCTIONS FOR MAIN ANALYSIS
# =========================================================================

# Filters data based on gender, age, and selected columns
filter_data <- function(data, gender_choice, age_min, age_max, col_gender, col_age) {
  if (col_age == "") {
    stop("Age column not found in data.")
  }

  filtered_data <- data %>%
    filter(!!rlang::sym(col_age) >= age_min & !!rlang::sym(col_age) <= age_max)

  if (col_gender != "" && col_gender %in% names(data)) {
    filtered_data <- filtered_data %>%
      mutate(Gender_Standardized = case_when(
        grepl("male|m|man|jongen(s)?|heren|mannelijk(e)?", !!rlang::sym(col_gender), ignore.case = TRUE) ~ "Male",
        grepl("female|f|vrouw(en)?|v|meisje(s)?|dame|mevr|vrouwelijke", !!rlang::sym(col_gender), ignore.case = TRUE) ~ "Female",
        TRUE ~ "Other"
      ))

    if (gender_choice != "Both") {
      filtered_data <- filtered_data %>%
        filter(Gender_Standardized == case_when(
          gender_choice == "M" ~ "Male",
          gender_choice == "F" ~ "Female"
        ))
    }
  } else {
    filtered_data <- filtered_data %>%
      mutate(Gender_Standardized = "Combined")
  }

  return(filtered_data)
}

# Generates a unique and safe filename for plots
generate_safe_filename <- function(plot_title, base_path, extension = "png") {
  safe_title <- gsub("[^a-zA-Z0-9_-]", "_", plot_title)
  datestamp <- format(Sys.Date(), "%Y%m%d")
  timestamp <- format(Sys.time(), "%H%M%S")
  file.path(base_path, paste0(safe_title, "_", datestamp, "-", timestamp, ".", extension))
}

# Generates the refineR plot with optional manual reference limits
generate_refiner_plot <- function(model, title, xlab, ylab, ref_low, ref_high) {
  req(model)
  plot(model, showCI = TRUE, RIperc = c(0.025, 0.975), showPathol = FALSE,
       title = title,
       xlab = xlab,
       ylab = ylab)

  usr <- par("usr")
  y_max <- usr[4]
  y_label_pos <- y_max * 0.95

  if (!is.na(ref_low) && is.numeric(ref_low)) {
    abline(v = ref_low, col = "red", lty = 2, lwd = 2)
    text(x = ref_low, y = y_label_pos,
         labels = round(ref_low, 2),
         col = "red", cex = 1.1, pos = 4)
  }

  if (!is.na(ref_high) && is.numeric(ref_high)) {
    abline(v = ref_high, col = "blue", lty = 2, lwd = 2)
    text(x = ref_high, y = y_label_pos,
         labels = round(ref_high, 2),
         col = "blue", cex = 1.1, pos = 2)
  }
}

# =========================================================================
# MAIN SERVER LOGIC
# =========================================================================

mainServer <- function(input, output, session, data_reactive, selected_dir_reactive, message_rv, analysis_running_rv) {

  # Reactive values to hold the model and plot title
  refiner_model_rv <- reactiveVal(NULL)
  plot_title_rv <- reactiveVal("")

  # Helper function to guess column names
  guess_column <- function(cols_available, common_names) {
    for (name in common_names) {
      match_idx <- grep(paste0("^", name, "$"), cols_available, ignore.case = TRUE)
      if (length(match_idx) > 0) {
        return(cols_available[match_idx[1]])
      }
    }
    return("")
  }

  # Observer for file upload, which updates column selections
  observeEvent(input$data_file, {
    req(input$data_file)
    tryCatch({
      data <- readxl::read_excel(input$data_file$datapath)
      data_reactive(data)
      message_rv(list(type = "success", text = "Data file uploaded and loaded successfully."))

      col_names <- colnames(data)
      all_col_choices_with_none <- c("None" = "", col_names)

      updateSelectInput(session, "col_value", choices = all_col_choices_with_none, selected = guess_column(col_names, c("HB_value", "Value", "Result", "Measurement", "Waarde")))
      updateSelectInput(session, "col_age", choices = all_col_choices_with_none, selected = guess_column(col_names, c("leeftijd", "age", "AgeInYears", "Years")))
      updateSelectInput(session, "col_gender", choices = all_col_choices_with_none, selected = guess_column(col_names, c("geslacht", "gender", "sex", "Gender", "Sex")))
    }, error = function(e) {
      message_rv(list(type = "error", text = paste("Error loading file:", e$message)))
      data_reactive(NULL)
    })
  })

  # Observer for the Reset button
  observeEvent(input$reset_btn, {
    shinyjs::reset("data_file")
    data_reactive(NULL)
    refiner_model_rv(NULL)
    plot_title_rv("")
    message_rv(list(type = "", text = ""))

    updateSelectInput(session, "col_value", choices = c("None" = ""), selected = "")
    updateSelectInput(session, "col_age", choices = c("None" = ""), selected = "")
    updateSelectInput(session, "col_gender", choices = c("None" = ""), selected = "")
    updateRadioButtons(session, "model_choice", selected = "BoxCox")
    updateSliderInput(session, "nbootstrap_speed", value = 50)
  })

  # Observer for directory selection
  shinyFiles::shinyDirChoose(
    input, id = 'select_dir_btn',
    roots = c(home = '~', wd = '.'), session = session
  )

  # Updates the reactive value with the selected directory path
  observeEvent(input$select_dir_btn, {
    if (!is.integer(input$select_dir_btn)) {
      path <- shinyFiles::parseDirPath(c(home = '~', wd = '.'), input$select_dir_btn)
      if (length(path) > 0) {
        selected_dir_reactive(path)
        message_rv(list(type = "success", text = paste("Output directory selected:", path)))
      } else {
        selected_dir_reactive(NULL)
        message_rv(list(type = "warning", text = "Directory selection cancelled."))
      }
    }
  })

  # Filters and cleans the data based on user inputs
  filtered_data_reactive <- reactive({
    req(data_reactive(), input$col_value, input$col_age)
    if (input$col_value == "" || input$col_age == "") {
      return(NULL)
    }

    filtered_data <- filter_data(data_reactive(), input$gender_choice, input$age_range[1], input$age_range[2], input$col_gender, input$col_age)

    value_col_name <- input$col_value

    if (!value_col_name %in% names(filtered_data)) {
        message_rv(list(text = "Error: Selected value column not found after filtering.", type = "danger"))
        return(NULL)
    }

    original_rows_count <- nrow(filtered_data)

    cleaned_data <- filtered_data %>%
      mutate(!!rlang::sym(value_col_name) := as.numeric(as.character(!!rlang::sym(value_col_name)))) %>%
      filter(!is.na(!!rlang::sym(value_col_name)))

    removed_rows_count <- original_rows_count - nrow(cleaned_data)

    return(list(data = cleaned_data, removed_rows = removed_rows_count))
  })

  # Observer for the Analyze button, running the refineR model
  observeEvent(input$analyze_btn, {
    if (analysis_running_rv()) {
      message_rv(list(text = "Analysis is already running. Please wait or reset.", type = "warning"))
      return()
    }

    filtered_result <- filtered_data_reactive()
    req(filtered_result)

    filtered_data <- filtered_result$data

    if (nrow(filtered_data) == 0) {
      message_rv(list(text = "Filtered dataset is empty. Please adjust your filtering criteria.", type = "danger"))
      return()
    }

    shinyjs::disable("analyze_btn")
    shinyjs::runjs("$('#analyze_btn').text('Analyzing...');")

    analysis_running_rv(TRUE)
    session$sendCustomMessage('analysisStatus', TRUE)

    refiner_model <- NULL

    tryCatch({
      # Simplified logic: Use isolate() directly inside the logic to avoid an intermediate variable.
      final_model_choice <- isolate({
        if (input$model_choice == "AutoSelect") {
          data_to_analyze <- filtered_data[[input$col_value]]
          skew <- moments::skewness(data_to_analyze, na.rm = TRUE)
          if (abs(skew) > 0.5) {
            "modBoxCox"
          } else {
            "BoxCox"
          }
        } else {
          input$model_choice
        }
      })

      # Run the main RefineR function with the selected model
      refiner_model <- refineR::findRI(Data = filtered_data[[input$col_value]],
                                       NBootstrap = isolate(input$nbootstrap_speed),
                                       model = final_model_choice)

      if (is.null(refiner_model) || inherits(refiner_model, "try-error")) {
        stop("RefineR model could not be generated. Check your input data and parameters.")
      }

      refiner_model_rv(refiner_model)

      gender_text <- if (isolate(input$col_gender) == "") "Combined" else paste0("Gender: ", isolate(input$gender_choice))

      # Adjust plot title to include transformation model
      model_text <- switch(final_model_choice,
                           "BoxCox" = " (BoxCox Transformed)",
                           "modBoxCox" = " (modBoxCox Transformed)")

      plot_title_rv(paste0("Estimated Reference Intervals for ", isolate(input$col_value),
                           model_text,
                           " (", gender_text,
                           ", Age: ", isolate(input$age_range[1]), "-", isolate(input$age_range[2]), ")"))

      # If auto-save is enabled, save the plot to the selected directory
      if (isolate(input$enable_directory) && !is.null(selected_dir_reactive())) {
        filename <- generate_safe_filename("RefineR_Plot", selected_dir_reactive(), "png")
        png(filename, width = 800, height = 600)

        generate_refiner_plot(refiner_model, plot_title_rv(),
                              sprintf("%s [%s]", isolate(input$col_value), isolate(input$unit_input)),
                              isolate(input$y_axis_label),
                              isolate(input$ref_low), isolate(input$ref_high))

        dev.off()
        message_rv(list(text = paste0("Plot saved to ", selected_dir_reactive()), type = "success"))
      }

      message_rv(list(text = "Analysis complete!", type = "success"))

    }, error = function(e) {
      error_message <- paste("Analysis Error:", e$message)
      message_rv(list(text = error_message, type = "danger"))
      refiner_model_rv(NULL) # Set to NULL to clear plot and summary
      print(error_message)
    }, finally = {
      analysis_running_rv(FALSE)
      session$sendCustomMessage('analysisStatus', FALSE)
      shinyjs::enable("analyze_btn")
      shinyjs::runjs("$('#analyze_btn').text('Analyze');")
    })
  })

  # Renders the text summary reactively
  output$result_text <- renderPrint({
    req(refiner_model_rv())

    # Get the results from the reactive expression
    filtered_result <- filtered_data_reactive()

    # Print the number of rows removed
    if (!is.null(filtered_result$removed_rows)) {
      cat(paste0("Note: ", filtered_result$removed_rows, " rows were removed due to missing or invalid data.\n\n"))
    }

    print(refiner_model_rv())
  })


  # Renders the live-updating plot output that depends on reactive inputs
  output$result_plot <- renderPlot({
    refiner_model <- refiner_model_rv()
    req(refiner_model) # Requires the model to be present before plotting
    plot_title <- plot_title_rv()

    generate_refiner_plot(refiner_model, plot_title,
                          sprintf("%s [%s]", input$col_value, input$unit_input),
                          input$y_axis_label,
                          input$ref_low, input$ref_high)
  })
  
  # Download handler for the main analysis report (A4 formatted)
  output$download_main_report <- downloadHandler(
    filename = function() {
      paste0("RefineR_Main_Report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      model <- refiner_model_rv()
      if (is.null(model)) {
        pdf(file, width = 8.27, height = 11.69, paper = "a4")
        plot.new()
        text(0.5, 0.5, "No analysis has been run. Please run the analysis first.", cex = 1.2)
        dev.off()
        return()
      }
      
      # Start PDF device with A4 dimensions
      pdf(file, width = 8.27, height = 11.69, paper = "a4")
      
      # 1. Generate the plot and capture it as a grid object
      generate_refiner_plot(model, plot_title_rv(),
                            sprintf("%s [%s]", input$col_value, input$unit_input),
                            input$y_axis_label,
                            input$ref_low, input$ref_high)
      plot_grob <- grid.grab()
      
      # 2. Set up the layout on a new page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(4, 1, heights = unit(c(0.1, 0.5, 0.1, 0.3), "npc"))))
      
      # 3. Report Title
      grid.text("RefineR Analysis Report", vp = viewport(layout.pos.row = 1, layout.pos.col = 1), gp = gpar(fontsize = 20, fontface = "bold"))
      
      # 4. Draw the captured plot into the second row of the layout
      pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1))
      grid.draw(plot_grob)
      popViewport()
      
      # 5. Summary Title
      grid.text("Analysis Summary", vp = viewport(layout.pos.row = 3, layout.pos.col = 1), gp = gpar(fontsize = 16, fontface = "bold"))
      
      # 6. Formatted Summary Text
      format_summary <- function(model) {
        ri_data <- getRI(model, RIperc = c(0.025, 0.975), pointEst = "medianBS")
        
        lower_ri <- sprintf("%.2f (95%% CI: %.2f to %.2f)", ri_data$PointEst[1], ri_data$CILow[1], ri_data$CIHigh[1])
        upper_ri <- sprintf("%.2f (95%% CI: %.2f to %.2f)", ri_data$PointEst[2], ri_data$CILow[2], ri_data$CIHigh[2])
        
        params <- model$model$parameters
        
        # Defensively check if params are numeric before rounding
        lambda_val <- if(is.numeric(params$lambda)) round(params$lambda, 3) else "N/A"
        mu_val <- if(is.numeric(params$mu)) round(params$mu, 3) else "N/A"
        sigma_val <- if(is.numeric(params$sigma)) round(params$sigma, 3) else "N/A"
        shift_val <- if(!is.null(params$shift) && is.numeric(params$shift)) round(params$shift, 3) else "N/A"

        summary_lines <- c(
          "Reference Intervals:",
          paste("  Lower Limit (2.5%):", lower_ri),
          paste("  Upper Limit (97.5%):", upper_ri),
          "",
          "Model Parameters:",
          paste("  Transformation Model:", model$model$partrans$model),
          paste("  Sample Size (N):", length(model$data$orig)),
          paste("  Lambda:", lambda_val),
          paste("  Mu:", mu_val),
          paste("  Sigma:", sigma_val),
          paste("  Shift:", shift_val)
        )
        paste(summary_lines, collapse = "\n")
      }
      
      summary_text <- format_summary(model)
      grid.text(summary_text, vp = viewport(layout.pos.row = 4, layout.pos.col = 1), x = 0.05, just = "left", gp = gpar(fontfamily = "Courier", fontsize = 10))
      
      # Close the PDF device
      dev.off()
    }
  )
}