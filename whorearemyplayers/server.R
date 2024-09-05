#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(ffscrapr)
library(nflreadr)
library(shinydashboardPlus)
library(shinyvalidate)
library(shinyFeedback)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # helper ----

  clean_players <- function(df) {
    df <- df %>%
      purrr::reduce(dplyr::bind_rows) %>%
      dplyr::select(franchise_id, franchise_name, player_id, player_name, pos, team) %>%
      dplyr::filter(!is.na(player_name)) %>%
      dplyr::distinct() %>%
      dplyr::mutate(
        franchise_id = as.integer(franchise_id),
        player_id = as.integer(player_id)
      )
  }

  # variables ----
  # get current time in local timezone
  current_time <- Sys.time()

  # get users timezone
  user_timezone <- Sys.timezone()

  position_order <- c("QB", "RB", "WR", "TE", "DT", "DE", "ILB", "OLB", "LB", "CB", "SS", "FS", "DB")

  current_season <- nflreadr::get_current_season(TRUE)
  current_week <- nflreadr::get_current_week(TRUE)
  current_nfl_matchups <- nflreadr::load_schedules(current_season) %>%
    dplyr::filter(week == current_week)

  all_players <- reactiveVal(data.frame())
  all_leagues <- reactiveVal(data.frame())

  # validation

  validation <- InputValidator$new()

  # outputa ----

  output$window_title <- renderUI({
    titlePanel(
      tags$h1(
        tags$span("Who"),
        tags$span("Are"),
        tags$span("My"),
        tags$span("Players"),
        paste(" in the week", current_week, "matchups?")
      ),
      windowTitle = paste("WAMP week ", current_week)
    )
  })

  if (nrow(current_nfl_matchups) > 0) {
    current_nfl_matchups <- current_nfl_matchups %>%
      dplyr::mutate(
        label = paste(away_team, "@", home_team),
        # create game_time from gameday and gametime
        timestamp = as.POSIXct(paste(gameday, gametime), format = "%Y-%m-%d %H:%M", tz = "America/New_York"),
        timestamp = as.POSIXct(timestamp, tz = user_timezone),
        timestamp_label = paste(
          weekdays(timestamp, TRUE), format(timestamp, "%d.%m. %H:%M")
        ),
        # check if game is upcoming, running or finished
        color = case_when(
          timestamp > current_time ~ "primary",
          timestamp < current_time & timestamp + 60 * 60 * 3 > current_time ~ "success",
          timestamp + 60 * 60 * 3 < current_time ~ "danger"
        ),
        collapsed = ifelse(color == "success", FALSE, TRUE)
      ) %>%
      dplyr::arrange(color, timestamp)
      # TODO: check if sorting is correct

    nfl_weekly_roster <- nflreadr::load_rosters_weekly(current_season) %>%
      dplyr::filter(week == current_week) %>%
      dplyr::select(full_name, team, depth_chart_position, jersey_number, status) %>%
      dplyr::mutate(
        full_name = nflreadr::clean_player_names(full_name),
        team = nflreadr::clean_team_abbrs(team)
      )

    # render inputs dependent on platform ----
    observeEvent(input$platform, {
      req(input$platform)

      output$inputs <- renderUI({
        if (input$platform == "MFL") {
          platform_inputs <- tagList(
            textInput("username", "Enter platform username"),
            passwordInput("password", "Enter platform password")
          )
          validation$add_rule("username", shinyvalidate::sv_required())
          validation$add_rule("password", shinyvalidate::sv_required())
        } else if (input$platform == "Fleaflicker") {
          platform_inputs <- tagList(
            textInput("usermail", "Enter platform user mail adress")
          )
          validation$add_rule("usermail", shinyvalidate::sv_required())
          validation$add_rule("usermail", shinyvalidate::sv_email())
        } else if (input$platform == "Sleeper") {
          platform_inputs <- tagList(
            textInput("username", "Enter platform username")
          )
          validation$add_rule("username", shinyvalidate::sv_required())
        }

        if (input$platform != "") {
          tagList(
            platform_inputs,
            shinyFeedback::loadingButton("addPlatform", "add leagues"),
            tags$p("Depending on how many leagues you have in this account and the roster sizes this can take some time")
          )
        }
      })
    })

    # create main ui ----
    observeEvent(input$addPlatform, {
      if (validation$is_valid()) {
        tryCatch({
          id <- paste0(input$addPlatform) # todo: id fixen wenn mehrere gleiche platformen hinzugefügt werden

          if (input$platform == "MFL") {
            req(input$username, input$password)
            conn <- ffscrapr::mfl_connect(current_season, user_name = input$username, password = input$password)
            leagues <- ffscrapr::ff_userleagues(conn) %>%
              dplyr::mutate(platform = "MFL")

            # for each row in leagues get league players and combine them in all_players
            players <- lapply(1:nrow(leagues), function(i) {
              leagueConn <- ffscrapr::mfl_connect(current_season, user_name = input$username, password = input$password, league_id = leagues$league_id[i])
              ffscrapr::ff_rosters(leagueConn, current_week) %>%
                dplyr::filter(franchise_id == leagues$franchise_id[i])
            }) %>%
              clean_players()
          } else if (input$platform == "Fleaflicker") {
            req(input$usermail)

            leagues <- ffscrapr::fleaflicker_userleagues(current_season, user_email = input$usermail) %>%
              dplyr::mutate(platform = "Fleaflicker")

            # for each row in leagues get league players and combine them in all_players
            players <- lapply(1:nrow(leagues), function(i) {
              leagueConn <- ffscrapr::fleaflicker_connect(current_season, user_email = input$usermail, league_id = leagues$league_id[i])
              ffscrapr::ff_rosters(leagueConn, current_week) %>%
                dplyr::filter(franchise_id == leagues$franchise_id[i])
            }) %>%
              clean_players()
          } else if (input$platform == "Sleeper") {
            req(input$username)
            # TODO: cleanup wenn ffscrapr update kommen sollte, wo die franchise ids der roster und der spieler übereinstimmen

            leagues <- ffscrapr::sleeper_userleagues(current_season, user_name = "MC07") %>%
              dplyr::mutate(platform = "Sleeper")

            sleeper_players <- ffscrapr::sleeper_players() %>%
              dplyr::select(player_id, player_name, pos, team)


            # for each row in leagues get league players and combine them in all_players
            players <- lapply(1:nrow(leagues), function(i) {
              leagueConn <- ffscrapr::sleeper_connect(current_season, user_name = "MC07", league_id = leagues$league_id[i])
              ffscrapr::sleeper_getendpoint(glue::glue("league/{leagues$league_id[i]}/rosters")) %>%
                purrr::pluck("content") %>%
                tibble::tibble() %>%
                tidyr::hoist(1, "player_id" = "players", "franchise_id" = "owner_id") %>%
                tidyr::unnest("player_id") %>%
                dplyr::transmute(
                  franchise_id = as.character(.data$franchise_id),
                  player_id = as.character(.data$player_id)
                ) %>%
                dplyr::filter(franchise_id == leagues$franchise_id[i]) %>%
                dplyr::mutate(
                  franchise_name = leagues$league_name[i]
                )
            }) %>%
              purrr::reduce(dplyr::bind_rows) %>%
              dplyr::left_join(sleeper_players, by = "player_id") %>%
              dplyr::select(franchise_id, franchise_name, player_id, player_name, pos, team) %>%
              dplyr::filter(!is.na(player_name)) %>%
              dplyr::distinct()
          }

          current_players <- all_players()
          updated_players <- dplyr::bind_rows(current_players, players)
          all_players(updated_players)

          shinyFeedback::resetLoadingButton("addPlatform")

          #current_leagues <- all_leagues()
          #updated_leagues <- dplyr::bind_rows(current_leagues, leagues)
          #all_leagues(updated_leagues)


          # TODO: add sleeper

          #output$userLeagues <- renderUI({
          #  lapply(1:nrow(all_leagues()), function(i) {
          #    tags$div(
          #      class = "userTeam",
          #      paste0(leagues$league_name[i], " (", leagues$platform[i], ")")
          #    )
              #                 #actionButton(paste0("remove_", input$addPlatform), "Entfernen")
          #  })
          #})

          # check if all_players is not empty
          if (nrow(all_players()) > 0) {
            players <- all_players() %>%
              dplyr::filter(team != "FA") %>%
              dplyr::mutate(
                player_name = nflreadr::clean_player_names(player_name),
                team = nflreadr::clean_team_abbrs(team)
              )

      #       # show one card for each current nfl matchup
            output$dynamic_ui <- renderUI({
              tagList(
                #fluidRow(
                # todo: toggle all boxes
                #  actionButton("toggle_boxes", "Toggle all Matchups")
                #)
                #selectizeInput(
                #  "filterMatchups",
                #  "Filter Matchups",
                #  choices = current_nfl_matchups$label,
                #  multiple = TRUE,
                #  options = list(
                #    placeholder = "Please choose",
                #    onInitialize = I('function() { this.setValue(""); }')
                #  )
                # TODO: matchup filter
                #),
                #,
                fluidRow(
                  lapply(seq_len(
                    nrow(current_nfl_matchups)
                      #dplyr::filter(label %in% input$filterMatchups)
                  ), function(i) {
                    matchup_players <- players %>%
                      dplyr::filter(team == current_nfl_matchups$home_team[i] | team == current_nfl_matchups$away_team[i]) %>%
                      dplyr::select(player_name, team) %>%
                      dplyr::distinct() %>%
                      dplyr::left_join(
                        nfl_weekly_roster,
                        by = c("player_name" = "full_name", "team")
                      ) %>%
                      dplyr::select(jersey_number, dplyr::everything()) %>%
                      dplyr::arrange(
                        status,
                        factor(depth_chart_position, levels = position_order)
                      ) %>%
                      dplyr::rename(
                        Player = player_name,
                        Team = team,
                        Pos = depth_chart_position,
                        No = jersey_number,
                        Status = status
                      )

                      box(
                        title = paste(current_nfl_matchups$away_team[i], "@", current_nfl_matchups$home_team[i], "-", current_nfl_matchups$timestamp_label[i]),
                        tags$div(
                          fluidRow(
                            column(6,
                                   renderTable(
                                     matchup_players %>%
                                       dplyr::filter(Team == current_nfl_matchups$away_team[i])
                                   )
                            ),
                            column(6,
                                   renderTable(
                                     matchup_players %>%
                                       dplyr::filter(Team == current_nfl_matchups$home_team[i])
                                   )
                            )
                          )
                        ),
                        width = 12,
                        status = current_nfl_matchups$color[i],
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed = current_nfl_matchups$collapsed[i],
                        id = "game_box"
                      )
                  })
                )
              )
            })
          } else {
            output$dynamic_ui <- renderUI({
              tags$div(
                class = "alert alert-danger",
                "No players found for current week"
              )
            })
          }

          observeEvent(input$toggle_boxes, {
            updateBox("game_box", action = "toggle")
          })
        }, error = function(e) {
          showNotification(
            "An error occures. Please try again.",
            id = "submit_message", type = "error"
          )
        })
      } else {
        ## if form is not valid ----
        # show error messages in form
        validation$enable()

        # reset llaoding button
        shinyFeedback::resetLoadingButton("addPlatform")

        # show notification
        showNotification(
          "Please correct the errors in the form and try again",
          id = "submit_message", type = "error"
        )
      }
    })
  } else {
    output$userLeagues <- renderUI({
      tags$p("As long we are in preseason, you can't import players.")
    })

    output$dynamic_ui <- renderUI({
      tags$div(
        class = "alert alert-danger",
        "No NFL matchups found for current week"
      )
    })
  }
}

