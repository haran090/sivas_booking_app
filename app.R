#####################################################################################################################################################################################################
#Bookings
#Last Updated : 30th August, 2020
#Author: Karthik
#####################################################################################################################################################################################################
#Libraries used (install before executing)
library(shiny)
library(shinyjs)
library(RMySQL)
library(DT)

#####################################################################################################################################################################################################
#####################################################################################################################################################################################################
# Global Variables and functions
#####################################################################################################################################################################################################
# Vector containing the mandatory fields in the form
fieldsMandatory <- c("name", "gmail_id", "mobile")
#####################################################################################################################################################################################################
# Function that is called by the ui to mark a field as mandatory
labelMandatory <- function(label) {
    tagList(
        label,
        span("*", class = "mandatory_star")
    )
}
#####################################################################################################################################################################################################
# Using CSS to make the asterisk red
appCSS <-
    ".mandatory_star { color: red; }"
#####################################################################################################################################################################################################
##Fields to Store in the responses directory
fields <- c("name", "gmail_id", "mobile")
#####################################################################################################################################################################################################
## Function to save and load data from the 

options(mysql = list(
    "host" = "127.0.0.1",
    "port" = 3306,
    "user" = "root",
    "password" = "Cop12345"
))
databaseName <- "bookings"
table <- "appointments"

saveData <- function(data) {
    # Connect to the database
    db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
    # Construct the update query by looping over the data fields
    query <- sprintf(
        "INSERT INTO %s (%s) VALUES ('%s')",
        table, 
        paste(names(data), collapse = ", "),
        paste(data, collapse = "', '")
    )
    # Submit the update query and disconnect
    dbGetQuery(db, query)
    dbDisconnect(db)
}

loadData <- function() {
    # Connect to the database
    db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
    # Construct the fetching query
    query <- sprintf("SELECT * FROM %s", table)
    # Submit the fetch query and disconnect
    data <- dbGetQuery(db, query)
    dbDisconnect(db)
    data
}



#####################################################################################################################################################################################################
#####################################################################################################################################################################################################
##Shiny App
shinyApp(
    ui = fluidPage(
         
            shinyjs::useShinyjs(),
            shinyjs::inlineCSS(appCSS),
            
        
            titlePanel("Book a Virtual Appointment with Dr. Sivaramakrishnan"),
            div(id = "form",
            sidebarLayout(
                sidebarPanel(
                    helpText("You will receive an SMS with link to complete payment. After you complete payment you will receive an invitation on your Google ID"),
                    textInput( inputId =   "name", label = labelMandatory ("Name"), value =  "", placeholder = "Mohammed Ibrahim"),
                    textInput( inputId =   "gmail_id", label = labelMandatory ("Google Mail ID"), value =  "", placeholder = "xxxxx@gmail.com"),
                    textInput( inputId =   "mobile", label = labelMandatory ("Mobile Number"), value =  "", placeholder = "9895123456"),
                    actionButton("submit", "Request Payment Link", class = "btn-primary")
                 
                 
                ),
                
                
                
            mainPanel(
                    
                    imageOutput("dadPhoto")
                ))
            
            
          ),
          shinyjs::hidden( 
            div(id = "thank_you",
                h3("Thank you for your response. You will receive a text message with UPI payment link. After completing payment you will receive a meeting link on your Google Mail ID.")
              
              
                )
            ) 
          ),

    
    
    server = function(input, output, session) {
        
       output$dadPhoto <- renderImage({
                                        
                                        filename <- "dadPhoto.png"
                                        
                                        # Return a list containing the filename and alt text
                                        list(src = filename,
                                             alt = "dad's photo"
                                        )
                                        
                                    }, deleteFile = FALSE)
        
        
        observe({
            # check if all mandatory fields have a value
            mandatoryFilled <-
                vapply(fieldsMandatory,
                       function(x) {
                           !is.null(input[[x]]) && input[[x]] != ""
                       },
                       logical(1))
            mandatoryFilled <- all(mandatoryFilled)
            
            # enable/disable the submit button
            shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
        })
        # Whenever a field is filled, aggregate all form data
        formData <- reactive({
            data <- sapply(fields, function(x) input[[x]])
            data
        })
        
        # When the Submit button is clicked, save the form data
        observeEvent(input$submit, {
            saveData(formData())
            shinyjs::reset("form")
            shinyjs::hide("form")
            shinyjs::show("thank_you")
        })
        
    
    }
)