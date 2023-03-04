# An Interactive Recruitment Dashboard for the ANTARES lab's research in the psychological and behavioral effects of trauma in a day-to-day life (via anecdotes).
# Bracketed parts of this code are confidential, personal information from API data pulls from REDCap that can not be shared; if you would like to recreate the dashboard, pull your own token and URL from REDCap.

library(shiny)
library(shinythemes)
library(shinydashboard)

#!/usr/bin/env Rscript
install.packages("httr")

token <- "[]"
url <- "[]"
formData <- list("token"=token,
                 content='record',
                 action='export',
                 format='json',
                 type='flat',
                 csvDelimiter='',
                 rawOrLabel='raw',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 exportSurveyFields='false',
                 exportDataAccessGroups='false',
                 returnFormat='json'
)

response <- httr::POST(url, body = formData, encode = "form")
result <- httr::content(response)

df <- do.call(rbind.data.frame,result)

names <- unique(df$firstname)
age <- unique(df$age)
height <- unique(df$height)
handedness <- unique(df$handedness)
phone <- unique(df$phone)
meds <- unique(df$meds_list)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home",
             tabName = "home",
             icon = icon("house")
    ),
    
    menuItem("Triage",
             tabName = "triage",
             icon = icon("person")
    ),
    
    menuItem("Phone Screening",
             icon = icon("phone"),
             tabName = "phonescreening"
    ),
    
    menuItem("Scheduled",
             icon = icon("calendar"),
             tabName = "scheduled"
    ),
    
    menuItem("Enrolled",
             icon = icon("book"),
             tabName = "enrolled"
    )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "home",
            h2("Home")
    ),
    
    tabItem(tabName = "triage",
            h2("Triage"),
            fluidPage(
              selectInput("triage_name", "Name: ", choices = unique(df$firstname), selected = NULL),
              column(
                
                textOutput("name"),
                textOutput("age"),
                textOutput("height"),
                textOutput("weight"),
                textOutput("handedness"),
                textOutput("phone"),
                textOutput("meds"),
                textOutput("priorDiagnoses"),
                textOutput("copperIUD"),
                textOutput("hormonalStatus"),
                textOutput("implants"),
                textOutput("pregnant"),
                textOutput("braces"),
                textOutput("cesD"),
                textOutput("PCL"),
                textOutput("endorsedTraumas"),
                width=12)
            )
    ),
    
    tabItem(tabName = "phonescreening",
            h2("Phone Screening"),
            fluidPage(
              selectInput("screening_name", "Name: ", choices = unique(df$firstname), selected = NULL),
              column(
                textOutput("name2"),
                textOutput("phone2"),
                textOutput("messageOk"),
                textOutput("email"),
                textOutput("secureStatus"),
                textOutput("textPermission"),
                textOutput("screenStatus"),
                textOutput("cesD2"),
                width=12)
            )
    ),
    
    tabItem(tabName = "scheduled",
            h2("Scheduled"),
            fluidPage(
              selectInput("scheduled_name", "Name: ", choices = unique(df$firstname), selected = NULL),
              column(
                p("Email date 1: ",style="text-align:Left;color:black;background-color:lightgray;padding:50px;border-radius:10px"),
                width = 3),
              column(
                p("Email date 2: ",style="text-align:Left;color:black;background-color:lightgray;padding:50px;border-radius:10px"),
                width = 3),
              column(
                p("Email date 3: ",style="text-align:Left;color:black;background-color:lightgray;padding:50px;border-radius:10px"),
                width = 3),
              column(
                p("Email date 4: ",style="text-align:Left;color:black;background-color:lightgray;padding:50px;border-radius:10px"),
                width = 3)
            )
    ),
    
    tabItem(tabName = "enrolled",
            h2("Enrolled")
    )
  )
)

# Put them together into a dashboardPage
ui <- dashboardPage(
  skin = "blue",
  shinydashboard::dashboardHeader(title = "McLean Hospital"),
  sidebar,
  body
)

server <- function(input, output, session) {
  output$name <- 
    renderText({
      #selected_name <- df$firstname[df$firstname == input$triage_name]
      paste("Name: ", input$triage_name)
    })
  
  output$age <-
    renderText({
      selected_age <- df$age[df$firstname == input$triage_name]
      paste("Age: ", selected_age)
    })
  
  output$height <-
    renderText({
      selected_height <- df$height[df$firstname == input$triage_name]
      paste("Height: ", selected_height)
    })
  
  output$weight <-
    renderText({
      selected_weight <- df$weight[df$firstname == input$triage_name]
      paste("Weight: ", selected_weight)
    })
  
  output$handedness <-
    renderText({
      selected_handedness <- df$handedness[df$firstname == input$triage_name]
      paste("Handedness: ", selected_handedness)
    })
  
  output$phone <-
    renderText({
      selected_phone <- df$phone[df$firstname == input$triage_name]
      paste("Smartphone: ", selected_phone)
    })
  
  output$meds <-
    renderText({
      selected_meds <- df$meds_list[df$firstname == input$triage_name]
      paste("Meds: ", selected_meds)
    })
  
  output$priorDiagnoses <-
    renderText({
      selected_diagnoses <- df$prev_dx[df$firstname == input$triage_name]
      paste("Prior Dianoses: ")
    })
  
  output$copperIUD <-
    renderText({
      selected_copperIUD <- df$paragard[df$firstname == input$triage_name]
      paste("Copper IUD: ", selected_copperIUD)
    })
  
  output$hormonalStatus <-
    renderText({
      #selected_hormonalStatus <- df$hormonalstatus___1, df$hormonalstatus___2[df$firstname == input$triage_name]
      paste("Hormonal Status: ")
    })
  
  output$implants <-
    renderText({
      #selected_implants <- df$paragard[df$firstname == input$triage_name]
      paste("Copper IUD: ")
    })
  
  output$pregnant <-
    renderText({
      #selected_pregnant <- df$pregnant[df$firstname == input$triage_name]
      paste("Pregnant: ")
    })
  
  output$braces <-
    renderText({
      #selected_braces <- df$braces[df$firstname == input$triage_name]
      paste("Braces: ")
    })
  
  output$cesD <-
    renderText({
      #selected_cesD <- df$cesD[df$firstname == input$triage_name]
      paste("Ces-D: ")
    })
  
  output$PCL <-
    renderText({
      #selected_PCL <- df$PCL[df$firstname == input$triage_name]
      paste("PCL: ")
    })
  
  output$endorsedTraumas <-
    renderText({
      #selected_endorsedTraumas <- df$endorsedTraumas[df$firstname == input$triage_name]
      paste("Endorsed Traumas: ")
    })
  
  output$name2 <-
    renderText({
      selected_name2 <- df$firstname[df$firstname == input$screening_name]
      paste("Name: ", selected_name2)
    })
  
  output$phone2 <-
    renderText({
      selected_phone2 <- df$phone[df$firstname == input$screening_name]
      paste("Phone: ", selected_phone2)
    })
  
  output$messageOK <-
    renderText({
      selected_messageok <- df$messageok[df$firstname == input$screening_name]
      paste("Messages OK: ", selected_messageok)
    })
  
  output$email <-
    renderText({
      selected_email <- df$email[df$firstname == input$screening_name]
      paste("Email: ", selected_email)
    })
  
  output$secureStatus <-
    renderText({
      #selected_secureStatus <- df$___[df$firstname == input$screening_name]
      paste("Secure Status: ")
    })
  
  output$textPermission<-
    renderText({
      #selected_phone2 <- df$___[df$firstname == input$screening_name]
      paste("Text Permission: ")
    })
  
  output$screenStatus <-
    renderText({
      #selected_name2 <- df$___[df$firstname == input$screening_name]
      paste("Screen Status: ")
    })
  
  output$cesD2 <-
    renderText({
      #selected_phone2 <- df$___[df$firstname == input$screening_name]
      paste("Ces-D: ")
    })
}
shinyApp(ui, server)
