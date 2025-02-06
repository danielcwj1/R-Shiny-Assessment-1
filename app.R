library(rsconnect)
library(shiny)
library(DT)
library(tools)
library(readxl)
library(dplyr)
library(tidyr)

ui <- fluidPage(
  titlePanel("R Shiny Assignment"),
  tabsetPanel(
    tabPanel(
      "Input",
      sidebarLayout(
        sidebarPanel(
          h4("Input data"),
          p("Please upload your claims triangle and tail factor here."),
          p("To avoid errors, ensure that your Excel or Csv file should have 
            columns set in the following order: Loss Year, Development Year and 
            Amount of Claims Paid ($)."),
          fileInput(
            "file_upload",
            "Upload your Excel or Csv File here:",
            accept = c(".xlsx", ".csv")
          ),
          numericInput(
            "tail_factor",
            "Tail Factor:",
            value = 1.1,
            min = 0,
            step = 0.01
          )
        ),
        mainPanel(
          DTOutput("claims_data")
        )
      )
    ),
    tabPanel(
      "Output",
      sidebarPanel(
        h4("Results"),
        p("In this page, you will find the full claims paid table, along with
          each development years' ultimate claims."),
        p("Additionally, you can find a line graph of the respective table for
          each accident year, to assist with visualization.")
      ),
      mainPanel(
        h4("Cumulative Paid Claims ($)"),
        tableOutput("data_table"),
        h4("Line Graph"),
        plotOutput("data_plot")
      )
    )
  )
)

server <- function(input, output, session) {
  claims_data <- reactive({
    req(input$file_upload)
    extension <- file_ext(input$file_upload$datapath)
    file_data <- switch(
      extension,
      xlsx = read_xlsx(input$file_upload$datapath),
      csv = read.csv(input$file_upload$datapath, header = TRUE),
      stop("This file type is not supported. Please upload a Csv or Excel file.")
    )
    colnames(file_data) <- make.names(colnames(file_data))
    colnames(file_data) <- sub("\\.", "_", colnames(file_data)) # Changes . to _
    file_data <- rename(file_data, Loss_Year = 1, Development_Year = 2, Amount_of_Claims_Paid = 3)
    file_data
  })
  cumulative_triangle <- reactive({
    req(claims_data())
    cumulative_data <- claims_data() %>%
      arrange(Loss_Year, Development_Year) %>% # Arrange data: Loss_Year followed by Development_Year
      group_by(Loss_Year) %>% # Groups by Accident Year
      mutate(Cumulative_Paid_Claims = cumsum(Amount_of_Claims_Paid)) %>%  # Calculative cumulative sum paid
      ungroup()
    cumulative_data_table <- cumulative_data %>%
      select(Loss_Year, Development_Year, Cumulative_Paid_Claims) %>%
      pivot_wider(
        names_from = Development_Year,
        values_from = Cumulative_Paid_Claims
      ) # Column title apart from Loss Year is Development Year, Values in table are from Cumulative Paid Claims
    max_col <- ncol(cumulative_data_table) 
    colnames(cumulative_data_table) <- c("Loss Year", as.character(seq(1, max_col - 1))) # Change names of Columns to Loss Year and 1, 2, ...
    max_dy <- max(as.numeric(colnames(cumulative_data_table)[-1]), na.rm = TRUE) # Highest DY available
    for (dy in 1:(max_dy -1)) { # For loop to fill in missing values of cumulative triangle
      col_current <- as.character(dy)
      col_next <- as.character(dy + 1)
      if (col_next %in% colnames(cumulative_data_table)) {
        valid_rows_current <- !is.na(cumulative_data_table[[col_current]])
        valid_rows_next <- !is.na(cumulative_data_table[[col_next]])
        valid_rows <- valid_rows_current & valid_rows_next
        if (sum(valid_rows) > 0) {
          df <- sum(cumulative_data_table[[col_next]][valid_rows], na.rm = TRUE) / 
            sum(cumulative_data_table[[col_current]][valid_rows], na.rm = TRUE) # Calculates development factors
          cumulative_data_table <- cumulative_data_table %>%
            mutate(
              !!sym(col_next) := ifelse(is.na(.data[[col_next]]),
                                        .data[[col_current]] * df,
                                        .data[[col_next]])
            )
        }
      }
    }
    ultimate_col <- as.character(max_dy + 1)
    cumulative_data_table <- cumulative_data_table %>%
      mutate(
        !!sym(ultimate_col) := as.numeric(.data[[as.character(max_dy)]]) * input$tail_factor # Calculate final row by multiplying last DY available with tail factor
      )
    cumulative_data_table <- cumulative_data_table %>%
      mutate(across(- 'Loss Year', ~ replace_na(., 0))) %>%  # Replace NA's with 0
      mutate(`Loss Year` = as.integer(`Loss Year`)) %>%
      mutate(across(- 'Loss Year', ~ round(., 0))) %>%  # Round all numeric columns to 0 decimal places
      mutate(across(- 'Loss Year', ~ format(., big.mark = ",", scientific = FALSE))) %>%  # Format numeric columns with commas
      return(cumulative_data_table)
  })
  data_plot <- reactive({
    cumulative_data_plot <- cumulative_triangle()
    plot_data <- cumulative_data_plot[, -1] # Exclude Loss_Year column
    plot_data <- as.data.frame(lapply(plot_data, function(x) {
      x_char <- as.character(x) # Convert to character
      x_clean <- gsub(",", "", x_char) # Remove commas
      suppressWarnings(as.numeric(x_clean)) # Convert to numeric
    }))
    all_values <- as.matrix(plot_data)
    finite_values <- all_values[is.finite(all_values)]
    min_claim <- if (length(finite_values) > 0) min(finite_values, na.rm = TRUE) else 0 # Compute min claim for y axis limit
    max_claim <- if (length(finite_values) > 0) max(finite_values, na.rm = TRUE) else 1 # Compute max claim for y axis limit
    min_year <- 1 # First development year for x axis limit
    max_year <- ncol(plot_data) # Max x axis limit is the number of columns in data (Includes ultimate claims)
    function() {
      par(mar = c(5, 4, 4, 4) + 0.1)
      plot(1, type = "n",
           xlim = c(min_year, max_year),
           ylim = c(min_claim, max_claim * 1.1),
           xlab = "Development Year",
           ylab = "Cumulative Claims Amount ($)",
           main = "Cumulative Paid Claims Over Development Years")
      colors <- rainbow(nrow(plot_data)) # Set rainbow to cater to more or less than 3 Loss Years
      loss_years <- as.character(cumulative_data_plot$`Loss Year`)
      for (i in 1:nrow(plot_data)) {
        lines(seq_len(ncol(plot_data)),
              as.numeric(plot_data[i, ]),
              col = colors[i],
              lwd = 2)
      } # For loop to plot data
      
      legend("topleft", legend = loss_years, col = colors, lwd = 2, title = "Loss Year")
    }
  })
  output$claims_data <- renderDT({
    claims_data()
  },
  options = list(
    dom = 't',
    paging = FALSE
  ),
  rownames = FALSE)
  output$data_table <- renderTable({
    req(cumulative_triangle())
    cumulative_triangle()
  })
  output$data_plot <- renderPlot({
    req(data_plot())
    data_plot()()
  })
}

shinyApp(ui, server)