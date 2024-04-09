# Load required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(gridExtra)
library(DT)

# Define the user interface
ui <- dashboardPage(
  dashboardHeader(title = "Feedback Portal"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Feedback", tabName = "feedback"),
      menuItem("Survey Results", tabName = "results")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "feedback",
              fluidRow(
                # Display welcome message
                wellPanel(
                  tags$h2("Welcome to our Survey!"),
                  tags$h4("Your feedback is important to us")
                )),
              # Feedback submission form
              h2("Submit Your Feedback"),
              fluidRow(
                box(width= 10,
                    column(6,
                           selectInput("cleanliness", "Cleanliness:", choices = c(1, 2, 3, 4, 5), selected = 5),
                           selectInput("staff_friendliness", "Staff Friendliness:", choices = c(1, 2, 3, 4, 5), selected = 5),
                           selectInput("room_comfort", "Room Comfort:", choices = c(1, 2, 3, 4, 5), selected = 5)
                    ),
                    column(6,
                           selectInput("facilities", "Facilities:", choices = c(1, 2, 3, 4, 5), selected = 5),
                           selectInput("overall_experience", "Overall Experience:", choices = c(1, 2, 3, 4, 5), selected = 5),
                           textInput("comments", "Comments:")
                    ),
                    actionButton("submit", "Submit"))
              ),
              # File upload section
              fluidRow(
                box(
                  fileInput("upload_data", "Upload CSV File")
                )
              )
      ),
      tabItem(tabName = "results",
              # Survey results section
              h2("Survey Results"),
              fluidRow(
                # Display feedback ratings plot
                box(
                  title = "Feedback Ratings",
                  width = 6,
                  plotOutput("feedback_plot")
                ),
                # Display feedback comments table
                box(
                  title = "Feedback Comments",
                  width = 6,
                  DT::dataTableOutput("comments_table")
                ),
                # Download feedback data button
                box(
                  title = "Download Data",
                  width = 6,
                  downloadButton("download_feedback_data", "Download Feedback Data")
                ),
                # Download plot button
                box(
                  title= "Download Plot",
                  width= 6,
                  downloadButton("downloadPlot", "Download Plot")
                )
              )
      )
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Data frame to store feedback
  feedback_data <- reactiveVal(NULL)
  
  # Load data from uploaded CSV file
  observeEvent(input$upload_data, {
    if (!is.null(input$upload_data)) {
      uploaded_file <- input$upload_data
      if (tolower(tools::file_ext(uploaded_file$name)) == "csv") {
        # Read the uploaded CSV file
        data <- read.csv(uploaded_file$datapath, stringsAsFactors = FALSE)
        
        # Split the single column into separate columns
        feedback_columns <- strsplit(data[,1], ";")
        feedback_df <- as.data.frame(do.call(rbind, feedback_columns))
        colnames(feedback_df) <- c("cleanliness", "staff_friendliness", "room_comfort", "facilities", "overall_experience", "comments")
        
        # Ensure the comments column is read correctly
        if (ncol(feedback_df) < 6) {
          feedback_df$comments <- NA
        } else {
          feedback_df$comments <- feedback_df$comments
        }
        
        # Update the feedback data with the parsed data
        feedback_data(feedback_df)
        
        # Show modal dialog for successful data upload
        showModal(modalDialog(
          title = "Success!",
          "Data has been loaded successfully.",
          easyClose = TRUE
        ))
      } else {
        # Show modal dialog for error in file format
        showModal(modalDialog(
          title = "Error",
          "Please upload a CSV file.",
          easyClose = TRUE
        ))
      }
    }
  })
  
  # Update survey results when submit button is clicked
  observeEvent(input$submit, {
    new_feedback <- data.frame(
      cleanliness = input$cleanliness,
      staff_friendliness = input$staff_friendliness,
      room_comfort = input$room_comfort,
      facilities = input$facilities,
      overall_experience = input$overall_experience,
      comments = input$comments,
      stringsAsFactors = FALSE
    )
    current_feedback <- feedback_data()
    updated_feedback <- rbind(current_feedback, new_feedback)
    feedback_data(updated_feedback)
    
    # Show modal dialog for successful feedback submission
    showModal(modalDialog(
      title = "Thank You!",
      "Your feedback has been submitted.",
      easyClose = TRUE
    ))
  })
  
  # Generate feedback rating plots
  output$feedback_plot <- renderPlot({
    if (!is.null(feedback_data())) {
      # Create a bar plot for each feedback parameter
      p1 <- ggplot(feedback_data(), aes(x = cleanliness)) +
        geom_bar(fill = "skyblue", color = "black") +
        labs(title = "Cleanliness Rating", x = "Rating", y = "Count")
      
      p2 <- ggplot(feedback_data(), aes(x = staff_friendliness)) +
        geom_bar(fill = "lightgreen", color = "black") +
        labs(title = "Staff Friendliness Rating", x = "Rating", y = "Count")
      
      p3 <- ggplot(feedback_data(), aes(x = room_comfort)) +
        geom_bar(fill = "orange", color = "black") +
        labs(title = "Room Comfort Rating", x = "Rating", y = "Count")
      
      p4 <- ggplot(feedback_data(), aes(x = facilities)) +
        geom_bar(fill = "lightpink", color = "black") +
        labs(title = "Facilities Rating", x = "Rating", y = "Count")
      
      p5 <- ggplot(feedback_data(), aes(x = overall_experience)) +
        geom_bar(fill = "lightblue", color = "black") +
        labs(title = "Overall Experience Rating", x = "Rating", y = "Count")
      
      # Combine the bar plots into a single plot
      grid.arrange(p1, p2, p3, p4, p5, ncol = 1)
    }
  })
  
  # Display recent comments
  output$comments_table <- renderDataTable({
    if (!is.null(feedback_data())) {
      datatable(feedback_data()[, "comments", drop = FALSE], options = list(lengthChange = FALSE))
    }
  })
  
  # Download feedback data as CSV
  output$download_feedback_data <- downloadHandler(
    filename = function() {
      "feedback_data.csv"
    },
    content = function(file) {
      write.csv(feedback_data(), file)
    }
  )
  
  # Download Plot
  output$downloadPlot<- downloadHandler(
    filename = function(){
      paste0("plots", "png", sep=".")
    },
    content = function(file){
      png(file)
      p1 <- ggplot(feedback_data(), aes(x = cleanliness)) +
        geom_bar(fill = "skyblue", color = "black") +
        labs(title = "Cleanliness Rating", x = "Rating", y = "Count")
      
      p2 <- ggplot(feedback_data(), aes(x = staff_friendliness)) +
        geom_bar(fill = "lightgreen", color = "black") +
        labs(title = "Staff Friendliness Rating", x = "Rating", y = "Count")
      
      p3 <- ggplot(feedback_data(), aes(x = room_comfort)) +
        geom_bar(fill = "orange", color = "black") +
        labs(title = "Room Comfort Rating", x = "Rating", y = "Count")
      
      p4 <- ggplot(feedback_data(), aes(x = facilities)) +
        geom_bar(fill = "lightpink", color = "black") +
        labs(title = "Facilities Rating", x = "Rating", y = "Count")
      
      p5 <- ggplot(feedback_data(), aes(x = overall_experience)) +
        geom_bar(fill = "lightblue", color = "black") +
        labs(title = "Overall Experience Rating", x = "Rating", y = "Count")
      
      # Combine the bar plots into a single plot
      p<- grid.arrange(p1, p2, p3, p4, p5, ncol = 1)
      print(p)
      dev.off()
    }
  )
}

# Run the application
shinyApp(ui, server)
