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


server <- function(input, output) {
  start_time = Sys.time()

  observeEvent(input$get_code, {
    options("httr_oob_default" = T)
    
    cKey = "dj0yJmk9S2ZZaEE2YkJFSGJSJmQ9WVdrOVdVUkJWMkpQVG1NbWNHbzlNQT09JnM9Y29uc3VtZXJzZWNyZXQmc3Y9MCZ4PWMy"
    cSecret = "2c73fdf1a08472f286cddd46cf1fba3a1b4db065"
    
    yahoo <- httr::oauth_endpoint(authorize ="https://api.login.yahoo.com/oauth2/request_auth", 
                                  access = "https://api.login.yahoo.com/oauth2/get_token", 
                                  base_url = "https://fantasysports.yahooapis.com")
    
    myapp <- httr::oauth_app("yahoo", key=cKey, secret = cSecret, redirect_uri = "oob")
    
    nav_link = httr::oauth2.0_authorize_url(yahoo, myapp, scope="fspt-r", redirect_uri = myapp$redirect_uri)
    
    # output$link <- renderText(paste0(nav_link))
    
    output$link <- renderUI({
      tags$a(href = nav_link, "Click here", targget = "_blank")
    })
    
    # httr::BROWSE(httr::oauth2.0_authorize_url(yahoo, myapp, scope="fspt-r", redirect_uri = myapp$redirect_uri))
    # 
    observeEvent(input$auth, {
      passcode = input$code
      
      yahoo_token <- httr::oauth2.0_access_token(yahoo,myapp,code=passcode)
      
      league_id = "1274195"
      game_id = "414"
      
      url <- glue("https://fantasysports.yahooapis.com/fantasy/v2/league/{game_id}.l.{league_id}/settings")
      
      test <- GET(url, add_headers(Authorization =
                                     paste0("Bearer ", yahoo_token$access_token)))
      
      output$test <- renderText(test$status_code)
    })
    
  })

  bs <- reactive({
    req(input$file1)
    bs <- read_excel(input$file1$datapath, sheet = "BeerSheet_csv_updated")
    
    return(bs)
  })
  
  
  userinfo <- reactive({
    req(input$file1)
    userinfo <- read_excel(input$file1$datapath, sheet = "metadata")
    
    # userinfo$draft_url[1] = gsub("https://sleeper.com/draft/nfl/", "", userinfo$draft_url[1])
    userinfo$draft_url[1] = str_extract(userinfo$draft_url[1] , "[0-9]+")
    
    return(userinfo)
  })
  
  observeEvent(input$do, {
    req(input$file1)
    draft_id = userinfo()$draft_url[1]
    user_id = userinfo()$sleeper_userid[1]
    g_url = userinfo()$googlesheet_url[1]
    
    url <- glue("https://api.sleeper.app/v1/draft/{draft_id}/picks")
    
    draft <- GET(url) %>%
      content()
    
    m1 <- draft %>%
      map_df(magrittr::extract, c("round", "roster_id", "player_id", "picked_by",
                                  "pick_no", "draft_slot"))
    
    m2 <- draft %>%
      map("metadata") %>%
      map_df(magrittr::extract, c("player_id", "first_name", "last_name", "years_exp", 
                                  "team", "status", "position", "number",
                                  "injury_status"))
    
    dat <- left_join(m1, m2, by = "player_id")
    
    dat <- dat %>%
      mutate(key = paste0(first_name, " ", last_name, team),
             key = gsub("\\.|\\'", "", key))
    
    bs <- bs() %>%
      mutate(`drafted?` = ifelse(bs()$key %in% dat$key, 1, 0))
    
    # # Set authentication token to be stored in a folder called `.secrets`
    # options(gargle_oauth_cache = ".secrets")
    # 
    # # Authenticate manually
    # gs4_auth()
    # 
    # # Check that the non-interactive authentication works by first deauthorizing:
    # gs4_deauth()
    # 
    # Authenticate using token. If no browser opens, the authentication works.
    gs4_auth(cache = ".secrets", email = "statswithsasa@gmail.com")
    
    
    
    write_sheet(bs, ss = g_url, sheet = "BeerSheet_csv_updated")
    
    output$draft_id <- renderText({
      HTML(paste0("<b>Draft ID: </b>", draft_id))
    })
    
    output$user_id <- renderText({
      paste0("<b>Sleeper User ID: </b>", user_id)
    })
    
    output$last_pick <- renderText({
      paste0("<b>Last Updated Pick: </b>", max(dat$pick_no))
    })
    
    output$status <- renderText({
      paste0("<b>Last Updated: </b>", Sys.time())
    })
  })
  
  autoInvalidate <- reactiveTimer(15000)
  observe({
    autoInvalidate()
    if(Sys.time() <= start_time + 60) {
      cat(".")
      # output$test <- renderText(start_time)
    }

  })
  
  observe({
    autoInvalidate()

  })
  
}
