library(ggplot2)
library(anytime)
library(corrplot)

# Convert from UNIX Epoch Time to a Date string.
time_convert <- function(t) if(is.numeric(t) == T) anydate(t / 1000) else (as.numeric(as.POSIXct(t)) * 1000)

# Construct Result statistics dataframes for the Participant.
patient_table <- function(result) {
  # The mapping dataframe between Activity IDs and names.
  activity_map <- function(a) {
    map <- c()
    for (i in 1 : length(a))
      map <- cbind(map, c(a[i]$`id`, a[i]$name))
    setNames(data.frame(map), c('id', 'activity'))
  }
  
  # Get the Activity ID of a game or the Survey name (FIXME).
  activity_get <- function(chunk, map) {
    if(is.na(chunk$activity) == F)
      c(toString(map$activity[map$id == chunk$activity]), length(chunk$temporal_events), 'game')
    else c(chunk$static_data$survey_name, length(chunk$temporal_events), 'survey')
  }
  
  # Use separate dataframes for the game-type and survey-type Activity Results.
  game_table <- c()

  # Separate Activities and Results from the input.
  map <- activity_map(result$activities)
  info <- result$participant$result_events
  
  # Apply the appropriate summarizer function to the Activity type.
  for(i in 1 : nrow(info)) {
    chunk <- info[i,]
    activity <- activity_get(chunk, map)
    if(activity[3] == 'game'){
      if (activity[1] == 'Spatial Span') {
        if (chunk$static_data$type == '1') {
          game_table <- rbind(game_table, game_summary(chunk, 'Spatial Span Forward'))
        } else if (chunk$static_data$type == '2'){
          game_table <- rbind(game_table, game_summary(chunk, 'Spatial Span Backward'))
        }
      } else {
        game_table <- rbind(game_table, game_summary(chunk, activity[1]))
      }
    }
  }

  tables <- game_reform(game_table)
}

# Summarize all temporal events in the Result.
game_summary <- function(chunk,name) {
  temp <- chunk$temporal_events[[1]]
  if(is.null(temp)) return(NULL)
  if(nrow(temp) <= 1) return(NULL)
  
  # Unpack all temporal events into a single dataframe. (discard static data!)
  form <- c()
  for(i in 1 : nrow(temp)) {
    dat <- temp[i,]
    form <- rbind(form, c(name, dat$`item`, dat$type, dat$duration, dat$level, chunk$timestamp))
  }
  
  # Reformat the dataframe.
  form <- setNames(data.frame(form), c('name', 'item', 'correct', 'time', 'level', 'start'))
  form$item <- as.numeric(as.character(form$item))
  form$time <- as.numeric(as.character(form$time))
  form$level <- as.numeric(as.character(form$level))
  form$start <- time_convert(as.numeric(as.character(form$start)))
  
  form
}

game_reform <- function(game) {
  # Append statistics to the unique'd temporal events.
  cat = c('Spatial Span Forward', 'Spatial Span Backward', 'Jewels A', 'Jewels B')
  unique_t <- unique(game$start)
  time_table <- matrix(0, nrow <- length(unique_t), ncol <- length(cat) * 3)
  for(i in 1 : length(unique_t)) {
    for(j in 1 : length(cat)) {
      temp <- subset(game, start == unique_t[i] & name == cat[j])
      time_table[i, 3 * (j - 1) + 1]= mean(temp$correct == '1')
      time_table[i, 3 * (j - 1) + 2]= mean(temp$time / 1000)
      time_table[i, 3 * (j - 1) + 3]= sd(temp$time / 1000)
    }
  }
  
  # Create specific column names for the new statistics columns.
  column <- c()
  for (i in 1 : length(cat))
    column <- c(column, c(paste0(cat[i], '_accuracy'), paste0(cat[i], '_mtime'), paste0(cat[i], '_sdtime')))
  
  # Update and return the new dataframe.
  time_table <- setNames(data.frame(time_table), column)
  time_table$date <- unique_t
  row.names(time_table) <- NULL
  time_table
}

# Anonymous helper function to make a column-labeled dataframe.
make_df <- function(x, y, x_name = x, y_name = y, source) {
  index <- !is.na(source[[y]])
  return(setNames(data.frame(
    source[[x]][index],
    source[[y]][index]
  ), c(x_name, y_name)))
}

# 3 options in this function: accuracy, mean, sd
game_plot <- function(table,option) {
  name = c('Spatial Span Forward', 'Spatial Span Backward', 'Jewels A', 'Jewels B')
  if(option=='accuracy'){
    new_name=paste0(name,'_accuracy')
    title_name = "Average Accuracy"
    y_name = "Accuracy"
  }
  if(option=='mean'){
    new_name=paste0(name,'_mtime')
    title_name = "Mean Response Time"
    y_name = "second"
  }
  if(option=='sd'){
    new_name=paste0(name,'_sdtime')
    title_name = "SD of Response Time"
    y_name = "second"
  }
  # Make the DFs to be drawn in the chart.
  spatialF <- make_df('date', new_name[1], y_name = 'value', source = table)
  spatialF$map='Forward'
  spatialB <- make_df('date', new_name[2], y_name = 'value', source = table)
  spatialB$map='Backward'
  trailsA <- make_df('date',  new_name[3], y_name = 'value', source = table)
  trailsA$map='Trails A'
  trailsB <- make_df('date',  new_name[4], y_name = 'value', source = table)
  trailsB$map='Trails B'
  # Create a GGPlot with each line chart.
  ggplot() + 
    geom_line(data = spatialF, 
              aes(x = date, y = value, color = map)) +
    geom_line(data = spatialB, 
              aes(x = date, y = value, color = map)) +
    geom_line(data = trailsA, 
              aes(x = date, y = value, color = map)) +
    geom_line(data = trailsB, 
              aes(x = date, y = value, color = map)) +
    geom_point(data = spatialF, 
               aes(x = date, y = value, color = map)) +
    geom_point(data = spatialB, 
               aes(x = date, y = value, color = map)) +
    geom_point(data = trailsA, 
               aes(x = date, y = value, color = map)) +
    geom_point(data = trailsB, 
               aes(x = date, y = value, color = map)) +
    labs(title=title_name, x="Date", y=y_name) +
    scale_colour_manual("", values = c("Forward"="black","Backward"="red", 
                                       "Trails A"="blue","Trails B"="green"))
}

game_plot(patient_table(commandArgs()$data)$game,'accuracy')
