# 1. League ---------------------------------------------------------------


observe({
  updatePickerInput(session, "tl_league", choices = get_league(rv$df))
})


# 2. Team -----------------------------------------------------------------


# observe({
#   
#   updatePickerInput(session, "tt_league", choices = get_league(rv$df), selected = "La Liga")
# })
# 
# 
# observe({
#   updatePickerInput(session, "tt_team", choices = get_team(rv$df, x = input$tt_league), 
#                     selected = get_team_sample(rv$df, x = input$tt_league))
# })

observe({
  
  updatePickerInput(session, "tt_league", choices = get_league(rv$df), selected = get_league_sample(rv$df))
})


observe({
  updatePickerInput(session, "tt_team", choices = get_team(rv$df, x = input$tt_league))
})


# 3. Player ---------------------------------------------------------------


# Player 1
observe({
  updatePickerInput(session, "tp_league", choices = get_league(rv$df), 
                    selected = get_league_sample(rv$df))
})


observe({
  updatePickerInput(session, "tp_team", choices = get_team(rv$df, x = input$tp_league),
                    selected = (get_team(rv$df, x = input$tp_league)[sample(1:length(get_team(rv$df, x = input$tp_league)),1)]))
})

observe({
  updatePickerInput(session, "tp_player", choices = get_player(rv$df, x = input$tp_league, y = input$tp_team),
                    selected = (get_player(rv$df, x = input$tp_league, y = input$tp_team)[sample(1:length(get_player(rv$df, x = input$tp_league, y = input$tp_team)),1)]))
})


# Player 2
observe({
  updatePickerInput(session, "tp_league2", choices = get_league(rv$df),
                    selected = get_league_sample(rv$df))
})

observe({
  updatePickerInput(session, "tp_team2", choices = get_team(rv$df, x = input$tp_league2),
                    selected = (get_team(rv$df, x = input$tp_league2)[sample(1:length(get_team(rv$df, x = input$tp_league2)),1)]))
})

observe({
  updatePickerInput(session, "tp_player2", choices = get_player(rv$df, x = input$tp_league2, y = input$tp_team2),
                    selected = (get_player(rv$df, x = input$tp_league2, y = input$tp_team2)[sample(1:length(get_player(rv$df, x = input$tp_league2, y = input$tp_team2)),1)]))
})



# 4. Scout ----------------------------------------------------------------


observe({
  updatePickerInput(session, "ts_league", choices = get_league(rv$df))
})

observe({
  updatePickerInput(session, "ts_team", choices = get_team(rv$df, x = input$ts_league))
})

observe({
  updatePickerInput(session, "ts_player", choices = get_player(rv$df, x = input$ts_league, y = input$ts_team))
})

observe({
  updatePickerInput(session, "cl_cluster", choices = sort(unique(rv$df[,"Class"])))
})


observe({
  updateSliderInput(session, "s_value", min = get_transfer(rv$df, funs = min, var = "Values"), max = get_transfer(rv$df, funs = max, var = "Values"), value = get_transfer(rv$df, funs = mean, var = "Values")*3 )
})


observe({
  updateSliderInput(session, "s_age", min = get_transfer(rv$df, funs = min, var = "Age"), max = get_transfer(rv$df, funs = max, var = "Age"), 
                    value = get_transfer(rv$df, funs = mean, var = "Age")*1)
})

observe({
  updateSliderInput(session, "s_overall", min = get_transfer(rv$df, funs = min, var = "Overall"), max = get_transfer(rv$df, funs = max, var = "Overall"), 
                    value = get_transfer(rv$df, funs = mean, var = "Overall")*1)
})

observe({
  updatePrettyCheckboxGroup(session, "s_class", choices = sort(unique(rv$df[,"Class"])), selected = "Goal Keeper", inline = TRUE)
})

observe({
  updatePrettyCheckboxGroup(session, "s_league", choices = sort(unique(rv$df[,"League"])), selected = c("Bundesliga", "La Liga"), inline = TRUE)
})

observe({
  updatePrettyCheckboxGroup(session, "s_cluster", choices = sort(str_sub(unique(rv$GK_dt[,"Cluster"]), start = 2)), selected = 1:4, inline = TRUE)
})

# Correlation
observe({
  updatePickerInput(session, "cr_league", choices = get_league(rv$df), selected = "La Liga")
})

observe({
  updatePickerInput(session, "cr_class", choices = sort(unique(rv$df[,"Class"])), selected = "Forward")
})

observe({
  updatePickerInput(session, "cr_x", choices = get_picker_list(
    get_variables(df = rv$df, league = input$cr_league, class = input$cr_class)), selected = "Shot.Power"
  )
})

observe({
  updatePickerInput(session, "cr_y", 
                    choices = get_picker_list(get_variables(df = rv$df, league = input$cr_league, 
                                            class = input$cr_class)[get_variables(df = rv$df, league = input$cr_league, 
                                                                                  class = input$cr_class) != input$cr_x]),
                    selected = "Finishing" )
                    
})

# PCA

observe({
  updatePickerInput(session, "pca_class", choices = sort(unique(rv$df[,"Class"])))
})


