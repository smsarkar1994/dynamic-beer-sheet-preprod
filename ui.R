library(dplyr)
library(rvest)
library(httr)
library(jsonlite)
library(glue)
library(purrr)
library(readxl)
library(shiny)
library(googlesheets4)
library(stringr)
library(tidyr)
library(xml2)



ui <- fluidPage(
  
  # App title ----
  titlePanel(
    h1("Dynamic BeerSheet"), windowTitle = "Dynamic-Beersheet"
  ),
  
  titlePanel(
    h5("Author: Sameer Sarkar")
  ),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(position = "left",
                 h4("Select League Format"),
                 radioButtons("format", "",
                              choiceNames = list(
                                "Sleeper",
                                "Yahoo"
                              ), 
                              choiceValues = list(
                                "sleeper", "yahoo"
                              )),
                 conditionalPanel(condition = "input.format == 'yahoo'", 
                                  actionButton("get_code", "Get Code", class = "btn-primary"),
                                  uiOutput("link"),
                                  br(),
                                  textInput("code", "Enter Code"),
                                  actionButton("auth", "Authenticate", class = "btn-primary"),
                                  verbatimTextOutput("success"),
                                  br(),
                                  textInput("league_id", "Enter League ID"),
                                  verbatimTextOutput("test")),
                 br(),
                 # br(),
                 # br(),
                 h4("Download Custom BeerSheet Template"),
                 p(style = "font-size:125%;", a(href = "https://www.dropbox.com/s/kqp0y4nmtqcbzel/custom_beersheet_template.xlsx?dl=0",
                     'Click here for Dropbox download link.',
                     target="_blank"), style = "font-size:125%;"),
                 br(),
                 h4("Upload File", style = "font-size:125%;"),
                 fileInput("file1", "Choose Custom BeerSheet File (see instructions)",
                           multiple = TRUE),
                 br(),
                 actionButton("do", "Update BeerSheet", class = "btn-primary"),
                 htmlOutput("draft_id"),
                 htmlOutput("user_id"),
                 htmlOutput("last_pick"),
                 htmlOutput("status")
                 # accept = c("text/csv",
                 #            "text/comma-separated-values,text/plain",
                 #            ".csv")),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(position = "right",
              tabsetPanel(
                tabPanel("Description",
                         h2("Description"),
                         p(style = "font-size:125%;", "Welcome to my Dynamic BeerSheet App. Drafting in 
                         Fantasy is a frantic and stressful process. Luckily, there are 
                         lots of great draft kits out there to help us out -
                         in particular the fantastic BeerSheets created by Reddit
                         user /u/BeerSheets."),
                         p(style = "font-size:125%;margin-top:1%", "However, it can be tough to simultaneously juggle
                           the stress of the draft and update your BeerSheets
                           as players are being taken off the board. This is especially
                           hard if you have tight draft windows (<30s) or do in-person
                           drafts where you can be distracted by the antics of your leaguemates"),
                         p(style = "font-size:125%;margin-top:1%", "Enter my", strong("Dynamic BeerSheets App."), "This app is designed to 
                           integrate with your draft and", strong("dynamically update your BeerSheet 
                                                                   to show who's been drafted and who's still left.")),
                         p(style = "font-size:125%;margin-top:1%", "Note that this app currently
                         only works for", strong("Yahoo Leagues"), "(real-time drafts) and", strong("Sleeper Leagues"), "(both real-time drafts and mock drafts). 
                           It is likely not possible to add support for ESPN or NFL.com leagues."),
                         p(style = "font-size:125%;margin-top:1%", "You should have the following:"),
                         tags$ol(
                           tags$li(style = "font-size:125%;", "A Google Account (the dynamic spreadsheet will be a 
                           Google Sheet)."),
                           tags$li(style = "font-size:125%;", "Microsoft Excel (recommended but not required).")
                         ),
                         br(), 
                         p(style = "font-size:125%;margin-top:1%", strong("P.S."), "If you like this app, feel free to check out", 
                           a(href = "https://www.statswithsasa.com", 
                             'my blog', target="_blank"), "for other cool things. 
                           I write about everything ranging from sports to current events."),
                         p(style = "font-size:125%;margin-top:1%", strong("P.P.S."), "I'm hosting this app using a service that provides limited
                           free bandwidth. If you liked using this app, please consider",
                           a(href = "https://www.buymeacoffee.com/statswithsasa", 
                             "throwing me a few bucks", target="_blank"),
                           "to help with server costs."),
                         br(),
                         p(style = "font-size:125%;margin-top:1%", strong("P", tags$sup(3), ".S."),"Shoutout to the great folk(s) at BeerSheets and the awesome
                           work they do. As well as Sleeper for having a well-documented
                           and easy-to-use API.")
                         
                ),
                
                
                tabPanel("Instructions",
                         h2("Instructions"),
                         p(style = "font-size:125%;margin-top:1%", "To run this app, follow these instructions:"),
                         tags$ol(
                           tags$li(style = "font-size:125%;", "Select your league format in the side panel.",
                                   strong("For Yahoo leagues only,"), "also do the following"),
                           br(),
                           tags$ul(
                             tags$li(style = "font-size:125%;","Click the \"Get Code\" button and click on the link that appears below 
                             to generate an authentication code."),
                             tags$li(style = "font-size:125%;","Copy and paste this code into the \"Enter Code\" field."),
                             tags$li(style = "font-size:125%;","Click the \"Authenticate\" button. If the authentication succeeds,
                                     a message will pop up saying so. If not, reload the app and try again.
                                     If the authentication process succeeds", strong("DO NOT CLICK THIS BUTTON AGAIN or the app will crash.")),
                             tags$li(style = "font-size:125%;", "Enter your League ID. To find this, navigate to your League homepage -
                                     the League ID is the number after \"https://football.fantasysports.yahoo.com/f1/\".",
                                     a(href = "https://imgur.com/a/WkwpOfT", "It should look like this.", target="_blank"))
                           ),
                           br(),
                           tags$li(style = "font-size:125%", "Download the", strong("Custom BeerSheets Template"), "from the 
                           panel on the left."),
                           tags$li(style = "font-size:125%;margin-top:1%", "Go to the ", a(href = "https://footballabsurdity.com/beersheet-request-form/", "BeerSheets site", target="_blank"),
                             "and request a BeerSheet based on your league settings."),
                           tags$li(style = "font-size:125%;margin-top:1%", "Download ", strong("BOTH"), 
                                   "the .csv and .xlsx files from BeerSheets."),
                           tags$li(style = "font-size:125%;margin-top:1%", 'Paste the .csv file into the "BeerSheet_csv_raw" tab and the .xlsx file 
                           into the "Beersheet_xlsx_raw" tab of the template. Take care to', strong("completely overwrite"),
                             "anything in those tabs. I recommend copy and pasting the entire sheet from the source."),
                           br(),
                           tags$ul(
                             tags$li(style = "font-size:125%;", "If you don't have excel, skip to steps 5-7, then import the .csv & .xlsx BeerSheets 
                                     into separate Google Sheets as well. Paste them into the uploaded Google Sheet
                                     created in steps 5-6 and edit the \"metadata\" tab as described in step 8 
                                     in the Google Sheet itself instead. You can then download this sheet and upload
                                     it to the app without ever using excel.")
                           ),
                           br(),
                           tags$li(style = "font-size:125%;", "Navigate to", a(href = "https://drive.google.com/drive/my-drive", "Google Drive", target="_blank"),
                             "and create a new Google Sheet."),
                           tags$li(style = "font-size:125%;margin-top:1%", "In the new Google Sheet, upload the Formatted Custom BeerSheet by 
                           navigating to File -> Import -> Upload and selecting the formatted custom excel file."),
                           tags$li(style = "font-size:125%;margin-top:1%", "Edit the Google Sheet permissions so anyone can edit it. You can do so by clicking the green \"Share\"
                           button on the top right -> click \"save\" -> under \"General Access\", 
                           change it so \"Anyone with the link\" is an editor", strong("(not just a viewer)."),
                                   "Your screen should look like", a(href = "https://www.imgur.com/a/5p66xYt",
                                                                     "this.", target = "_place")),
                           br(),
                           tags$ul(
                             tags$li(style = "font-size:125%;", "The app WILL NOT work if you don't do this step.")
                           ),
                           br(),
                           tags$li(style = "font-size:125%;", 'In the "metadata" tab of the', strong("Excel file (not the Google Sheet)"),
                             "update the 3 fields with a link to your Sleeper draft room and your Sleeper username (Sleeper leagues only), 
                             as well as a link to the Google Sheet you just created (all league formats)."),
                           br(),
                           tags$ul(
                             tags$li(style = "font-size:125%;", "Note: Do NOT paste them in the first row, instead
                                     overwrite the \"placeholders\" in the second row. Keep the first row as is.")
                           ),
                           br(),
                           tags$li(style = "font-size:125%;", "Upload the Formatted Custom BeerSheet using the side panel 
                                   on the left."),
                           tags$li(style = "font-size:125%;margin-top:1%", "You're ready to go! Click the \"Update BeerSheet\"
                           button whenever you want to update the Google Sheet
                           with your draft progress. If the app has successfully executed,
                           some information about your draft and when the Dynamic BeerSheet
                           was last updated will display on the sidepanel.")
   
                         ),
                         br(),
                         p(style = "font-size:125%;margin-top:1%", strong("Note:"), "If your screen greys out at any time, 
                           it means the app has crashed or your session has timed out due to inactivity. 
                           Refresh it and make sure you've followed every step of these instructions. 
                           The app will not work until your live draft or mock has actually begun."),
                         p(style = "font-size:125%;margin-top:1%", "If the session has timed out, all you need to do is refresh and reupload
                           the Formatted Custom BeerSheet on the side panel to keep using the app. Nothing further
                           is required to keep using the app after the initial setup.")
                         
                )
              )
              
    )
  )
)