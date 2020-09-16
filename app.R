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
library(stringr)
library(DBI)
library(reticulate)
#####################################################################################################################################################################################################
#####################################################################################################################################################################################################
# Global Variables and functions
#####################################################################################################################################################################################################
# Vector containing the mandatory fields in the form
fieldsMandatory <- c("name", "gmail_id", "mobile")
#####################################################################################################################################################################################################
# Function that is called by the ui to mark a field as mandatory
labelMandatory <- function(label) {
    shiny::tagList(
        label,
        shiny::span("*", class = "mandatory_star")
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
    db <- DBI::dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
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
    DBI::dbGetQuery(db, query)
    DBI::dbDisconnect(db)
}

loadData <- function() {
    # Connect to the database
    db <- DBI::dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
    # Construct the fetching query
    query <- sprintf("SELECT * FROM %s", table)
    # Submit the fetch query and disconnect
    data <- DBI::dbGetQuery(db, query)
    DBI::dbDisconnect(db)
    data
}
#####################################################################################################################################################################################################
##Check if input is gmail id

valGmail <- function(x){
    return(stringr::str_detect(x, '^[a-z0-9](\\.?[a-z0-9]){5,}@g(oogle)?mail\\.com$' ))
}

#####################################################################################################################################################################################################
##Check if 10 digit mobile number

valMobile <- function(x){
    return(stringr::str_detect(x, '^[0-9]{10}$' ))
}


#####################################################################################################################################################################################################
#Sourcing python script and ensure dependencies exist; this makes createLink() and sendSMS available in the environment

reticulate::source_python("setu.py")



#####################################################################################################################################################################################################
##Shiny App
shiny::shinyApp(
    ui = shiny::fluidPage(
         #Landing Page
            shinyjs::useShinyjs(),
            shinyjs::inlineCSS(appCSS),
            
        
            shiny::titlePanel("Book a Virtual Appointment with Dr. Sivaramakrishnan"),
            shiny::div(id = "form",
            shiny::sidebarLayout(
                shiny::sidebarPanel(
                    shiny::helpText("You will receive an SMS with link to complete payment. After you complete payment you will receive an invitation on your Google ID"),
                    shiny::textInput( inputId =   "name", label = labelMandatory ("Name"), value =  "", placeholder = "Mohammed Ibrahim"),
                    shiny::textInput( inputId =   "gmail_id", label = labelMandatory ("Google Mail ID"), value =  "", placeholder = "xxxxx@gmail.com"),
                    
                    shiny::textInput( inputId =   "mobile", label = labelMandatory ("Mobile Number"), value =  "", placeholder = "9895123456"),
                    shiny::actionButton("submit", "Request Payment Link", class = "btn-primary")
                 
                 
                ),
                
                
                
                shiny::mainPanel(
                    
                    shiny::imageOutput("dadPhoto")
                ))
            
            
          ),
          #Link_page
          shinyjs::hidden( 
              shiny::div(id = "link_page",
                 shiny::h3("Thank you for your response. You can complete payment through the link below OR the link in the SMS."),
                 shiny::conditionalPanel(
                              condition = "input.submit == true",
                              uiOutput("payment_url")
                 )
              
                )
            ) 
          ),

    
    
    server = function(input, output, session) {
        
       output$dadPhoto <- shiny::renderImage({
                                        
                                        filename <- "dadPhoto.png"
                                        
                                        # Return a list containing the filename and alt text
                                        list(src = filename,
                                             alt = "dad's photo"
                                        )
                                        
                                    }, deleteFile = FALSE)
        
        
       shiny::observe({
            # check if all mandatory fields have a value
            mandatoryFilled <-
                vapply(fieldsMandatory,
                       function(x) {
                           !is.null(input[[x]]) && input[[x]] != ""
                       },
                       logical(1))
            mandatoryFilled <- all(mandatoryFilled)
            
            #Check if email is gmail id
            
            gmailCheck <- valGmail(input$gmail_id)
            
            #Check if mobile number is 10 digits
            
            mobileCheck <- valMobile(input$mobile)
            
            # enable/disable the submit button
            shinyjs::toggleState(id = "submit", condition = (mandatoryFilled && gmailCheck && mobileCheck))
        })
        
      
       
        
        
        # Whenever a field is filled, aggregate all form data
        formData <- shiny::reactive({
            data <- sapply(fields, function(x) input[[x]])
            data
        })
        
        #Creating dummy variable for display in the UI
        
        
        
        
        # When the Submit button is clicked, save the form data
        shiny::observeEvent(input$submit, {
            ##Writing entered data to the db
            saveData(formData())
           
            ##Resetting the form
            shinyjs::reset("form")
          
            ##Hiding the form
            shinyjs::hide("form")
          
            ##Calling createLink to create a payment link 
            link_details <- createLink()
            
            ##Parsing the contents of the list returned by createLink function
            payment_url <- link_details$url
            bill_id  <- link_details$billID
            
            ##Storing upi_link as an output object
            
            ###Creating a hyperlink text for display in HTML
            payment_link_url <- a("Click Here to Pay", href = payment_url)
            
            ##Creating an output object to display
            
            output$payment_url <- renderUI({
              tagList(payment_link_url)
            })
            
            #Send SMS with link details
            
            sendSMS(as.character(input$mobile), as.character(payment_url))
            
            
            ##Show link_page
            shinyjs::show("link_page")
        })
        

    }
)
