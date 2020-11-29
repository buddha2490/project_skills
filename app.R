library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(shinyalert)
library(DT)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(stringr)
library(RSQLite)
rm(list=ls())


ui <- fluidPage(title = "Directory",
                h2("Staff directory and project database", align = "center"),
                theme = shinytheme("darkly"),
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "mystylesheet.css")),
                useShinyjs(),
                shinyalert::useShinyalert(),
                navbarPage(title = NULL,
                           windowTitle = "Window title",
                           tabPanel("Staff directory",
                                    h4("View and edit the staff directory", align = "center"),
                                    HTML("<br>"),
                                    verticalLayout(
                                    div(class="button-container", align="right",
                                        circleButton("add_button", icon = icon("plus"), status = "success",
                                                     size = "sm", title = "label"),
                                    helpText("Click to add a new record", align = "right"))),
                                    HTML("<br>"),
                                    helpText("Please click on a 'view' icon to view selected record and the 'edit' button to add to the record"),
                                    dataTableOutput("mytable")),
                           tabPanel("Project database",
                                    h4("Project text goes here")))
)


                   


server <- function(input, output, session) {
  
  myDB <- dbConnect(RSQLite::SQLite(), "analysts.DB")
  

  
  analysts_df <- reactive({
    
    # Reactive to:
    input$add_button
    input$info_button
    input$edit_button
    
    analysts <- dbReadTable(myDB, "analysts")
    analysts$HireDate <- as.Date(analysts$HireDate, origin = "1970-01-01")
    analysts$Email <- str_replace(analysts$Email, 
                                  analysts$Email, 
                                  sprintf('<a href="mailto:%s">%s</a>',  analysts$Email, analysts$Email))
    analysts$view <- paste("<button id=\"info_button\" 
                                              type=\"button\" 
                                              class=\"btn btn-primary btn-sm\"
                                              onclick=\"Shiny.onInputChange(&quot;info_button&quot;,  Math.random())\"><i class=\"fa fa-address-card\"></i></button>")
    analysts$actions <-paste("<button id=\"edit_button\" 
                           type=\"button\" 
                           class=\"btn btn-link btn-sm\"
                           onclick=\"Shiny.onInputChange(&quot;edit_button&quot;,  Math.random())\"><i class=\"fa fa-edit fa-2x\"></i></button>") 
    
    
    
    analysts <- dplyr::select(analysts, View = view, Name, Title, Department, Email, Level, HireDate, actions, Bio, row_id)
    return(analysts)
  })
  

# View a record -----------------------------------------------------------
  #  This "View" page will need to be formatted in some nice manner using standard UI code
  #  This is also where we will include whatever skill information is included in the skills table
  observeEvent(input$info_button, {
    
    sel_row <- input$mytable_row_last_clicked
    row_id <- analysts_df()[sel_row, "row_id"]
    table <- dbReadTable(myDB, "analysts")
    
    # Profile values
    title <- table[table$row_id == row_id, "Title"]
    hire_date <- table[table$row_id == row_id, "HireDate"] %>%
      as.Date(origin = "1970-01-01")
    tenure <- (Sys.Date() - hire_date) %>% as.numeric()
    years <- floor(tenure / 365)
    days <- tenure %% 365
    time <- paste(years, "years and", days,"days")
    
    name <- table[table$row_id == row_id, "Name"]
    email <- table[table$row_id == row_id, "Email"]
    department <- table[table$row_id == row_id, "Department"]
    
    bio <- table[table$row_id == row_id, "Bio"]
    
    # I'm doing this with html tags, but we could create an htmlTemplate() within the 
    # fluidPage() to be much nicer
    showModal(
      modalDialog(id = "profile_form",
                  title = NULL,
                  footer = modalButton("Dismiss"),
                  easyClose = TRUE,
                  div(
                    fluidPage(
                      theme = shinytheme("darkly"),
                      HTML('<center><img src="ACS.png"></center>'),
                      h3("Employee information", align = "center"),
                      h4(name, align = "center"),
                      h5(title, align = "center"),
                      h5(tags$a(href=sprintf("mailto:%s", email), email)),
                      h5(department),
                      h5(paste("With ACS for",time),
                      HTML("<br><br>"),
                      verticalLayout(
                      h4(strong("Bio")),
                      h5(bio))
                      )))))
  })
  
  
  
  # Edit a record -----------------------------------------------------------
 # This will edit a record.  Sort of does the same thing we had before, only in the mondal window 
  observeEvent(input$edit_button, {
    
    
    sel_row <- input$mytable_row_last_clicked
    row_id <- analysts_df()[sel_row, "row_id"]
    table <- dbReadTable(myDB, "analysts")
    
    # Profile values
    title <- table[table$row_id == row_id, "Title"]
    name <- table[table$row_id == row_id, "Name"]
    email <- table[table$row_id == row_id, "Email"]
    department <- table[table$row_id == row_id, "Department"]
    
    bio <- table[table$row_id == row_id, "Bio"]
    
    
    
    showModal(
      modalDialog(id = "profile_form",
                  title = NULL,
                  footer = modalButton("Dismiss"),
                  easyClose = TRUE,
                  div(
                    fluidPage(
                      theme = shinytheme("darkly"),
                      splitLayout(
                      textInput(inputId = "newname",
                                label = "Name",
                                value = name),
                      textInput(inputId = "newtitle",
                                label = "Job title",
                                value = title)),
                      splitLayout(
                      textInput(inputId = "newemail",
                                label = "Email address",
                                value = email),
                      textInput(inputId = "newdepartment",
                                label = "Department",
                                value = department)
                      ),
                      HTML("<br>"),
                      textAreaInput(inputId = "newbio",
                                    label = "Introduce yourself (bio)",
                                    value = bio,
                                    height = '400px'),
                      actionButton("save_button", "Update your information", icon=icon("save"),
                                   status = "success")
                    )
                  )
      ))
    
    observeEvent(input$save_button, {
      
      oldtable <- dbReadTable(myDB, "analysts") 
      
      newname <- input$newname
      newtitle <- input$newtitle
      newemail <- input$newemail
      newdepartment <- input$newdepartment
      newbio <- input$newbio
      
      newtable <- oldtable[oldtable$row_id == row_id,]
      newtable$Name <- newname
      newtable$Title <- newtitle
      newtable$Email <- newemail
      newtable$Bio <- newbio
      newtable$Department <- newdepartment
      
      oldtable <- oldtable[oldtable$row_id != row_id,]
      final <- bind_rows(oldtable,newtable)
      final <- final[order(final$row_id),]
      dbWriteTable(myDB, "analysts", final, overwrite = T)
      session$reload()
      
    })
  })
  

# Add a record ------------------------------------------------------------
# This will need a bit of work, since we need to include all the things in the database
# But for a first pass this isn't too bad

  observeEvent(input$add_button, {

    showModal(
      modalDialog(id = "new_form",
                  title = NULL,
                  footer = modalButton("Dismiss"),
                  easyClose = TRUE,
                  div(
                    fluidPage(
                      theme = shinytheme("darkly"),
                      splitLayout(
                        textInput(inputId = "addname",
                                  label = "Preferred name",
                                  placeholder = "Jane Smith",
                                  value = ""),
                        textInput(inputId = "addtitle",
                                  label = "Job title",
                                  placeholder = "Scientist",
                                  value = "")
                      ),
                      HTML("<br><br>"),
                      splitLayout(
                        textInput(inputId = "adddepartment",
                                  label = "OCMSO Department",
                                  placeholder = "Patient services, Epidemiology, etc",
                                  value = ""),
                        textInput(inputId = "addemail",
                                  label = "E-mail address",
                                  placeholder = "jane.smith@cancer.org",
                                  value = "")
                                ),
                      HTML("<br><br>"),
                      splitLayout(
                        textInput(inputId = "addlevel",
                                  label = "Career level",
                                  placeholder = "E1, E2, etc",
                                  value = ""),
                        textInput(inputId = "addfunction",
                                  label = "Job function",
                                  placeholder = "Intramural Research",
                                  value = ""),
                        dateInput(inputId = "addhiredate",
                                  label = "Hire date",
                                  value = Sys.Date())
                      ),
                      HTML("<br><br>"),
                      textAreaInput(inputId = "addbio",
                                     label = "Tell us a little about yourself",
                                     placeholder = "I like cats more than dogs",
                                     height = '400px',
                                     value = ""),
                      HTML("<br><br>"),
                      actionButton(inputId = "save_new",
                                   label = "Save your information",
                                   icon = icon("save"),
                                   status = "success")
                    ))))

observeEvent(input$save_new, {

  table <- dbReadTable(myDB, "analysts")
  maxRow <- as.numeric(max(table$row_id))

  foo <- data.frame(Name = input$addname,
                    Title = input$addtitle,
                    Department = input$adddepartment,
                    Email = input$addemail,
                    Level = input$addlevel,
                    Function = input$addfunction,
                    HireDate = input$addhiredate,
                    row_id = as.character(maxRow + 1),
                    Bio = input$addbio)

  # Save it to the SQL file
  dbAppendTable(myDB, "analysts", foo)

  session$reload()
  

})

      })


 
  # Data table output -------------------------------------------------------
  
  output$mytable <- DT::renderDataTable({
    
    df <- analysts_df() %>% select(-row_id, -Bio)
    
    newTable <- DT::datatable(
      df,
      rownames = FALSE,
      escape = FALSE,
      selection = "single",
      options = list(searching = TRUE, 
                     lengthChange = FALSE,
                     pageLength = 20,
                     autoWidth = FALSE,
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': '#375a7f', 'color': '#fff'});",
                       "}")))
  })
  
  
}
shinyApp(ui, server)