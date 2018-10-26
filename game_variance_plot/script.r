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
  survey_table <- c()
  
  # Separate Activities and Results from the input.
  map <- activity_map(result$activities)
  info <- result$participant$results
  
  # Apply the appropriate summarizer function to the Activity type.
  for(i in 1 : nrow(info)) {
    chunk <- info[i,]
    activity <- activity_get(chunk, map)
    if(activity[3] == 'game')
      game_table <- rbind(game_table, game_summary(chunk, activity[1]))
    if(activity[3] == 'survey')
      survey_table <- rbind(survey_table, survey_summary(chunk))
  }
  tables <- list('survey' = survey_reform(survey_table), 'game' = game_reform(game_table))
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
    form <- rbind(form, c(name, dat$`item`, dat$type, dat$elapsed_time, dat$level, chunk$start_time))
  }

  # Reformat the dataframe.
  form <- setNames(data.frame(form), c('name', 'item', 'correct', 'time', 'level', 'start'))
  form$item <- as.numeric(as.character(form$item))
  form$time <- as.numeric(as.character(form$time))
  form$level <- as.numeric(as.character(form$level))
  form$start <- time_convert(as.numeric(as.character(form$start)))

  form
}

# same logic as game_summary
survey_summary=function(chunk){
  temp <- chunk$temporal_events[[1]]
  if(is.null(temp)) return(NULL)
  if(nrow(temp) <= 1) return(NULL)
  
  form=c()
  for(i in 1:nrow(temp)){
    dat = temp[i,]
    form=rbind(form,c(dat$`item`,dat$value,dat$elapsed_time,chunk$start_time))
  }
  form = data.frame(form)
  colnames(form)=c('question','answer','time','start')
  
  q=c(
  "Last night I had trouble falling asleep",                          
  "Last night I had trouble staying asleep",                               
  "This morning I was up earlier than I wanted",                           
  "In the last THREE DAYS, I have taken my medications as scheduled",      
  "In the last THREE DAYS, during the daytime I have gone outside my home",
  "In the last THREE DAYS, I have preferred to spend time alone",          
  "In the last THREE DAYS, I have had arguments with other people",        
  "Today I have heard voices or saw things others cannot",                 
  "Today I have had thoughts racing through my head",                      
  "Today I feel I have special powers",                                   
  "Today I feel people are watching me",                                  
  "Today I feel people are against me",                                    
  "Today I feel confused or puzzled",                                      
  "Today I feel unable to cope and have difficulty with everyday tasks" ,  
  "In the last THREE DAYS, I have had someone to talk to",                 
  "In the last THREE DAYS, I have felt uneasy with groups of people",      
  "Today I feel little interest or pleasure",                              
  "Today I feel depressed" ,                                               
  "Today I had trouble sleeping" ,                                         
  "Today I feel tired or have little energy"  ,                            
  "Today I have a poor appetite or am overeating"   ,                      
  "Today I feel bad about myself or that I have let others down"  ,        
  "Today I have trouble focusing or concentrating" ,                       
  "Today I feel too slow or too restless" ,                                
  "Today I have thoughts of self-harm",                                    
  "Today I feel anxious" ,                                                 
  "Today I cannot stop worrying" ,                                         
  "Today I am worrying too much about different things",                   
  "Today I have trouble relaxing"  ,                                       
  "Today I feel so restless it's hard to sit still"  ,                     
  "Today I am easily annoyed or irritable" ,                               
  "Today I feel afraid something awful might happen" )
  cat = c("sleep","sleep","sleep","medication", "social", "social","social","psychosis","psychosis", 
          "psychosis","psychosis","psychosis","psychosis","psychosis","social","social" ,"depression","depression",
          "depression" ,"depression", "depression", "depression", "depression","depression","depression","anxiety",
          "anxiety","anxiety","anxiety","anxiety","anxiety","anxiety" )
  survey_key=data.frame(cbind(q,cat))
  colnames(survey_key)=c("question","category")
  name=rep(NA,nrow(form))
  for(i in 1:nrow(form)){
    name[i]=as.character(survey_key$category)[survey_key$question==as.character(form$question)[i]]
  }
  form$name = name
  form$time = as.numeric(as.character(form$time))
  form$start = time_convert(as.numeric(as.character(form$start)))
  form
}

game_reform <- function(game) {
  # Append statistics to the unique'd temporal events.
  cat = c('Spatial Span Forward', 'Spatial Span Backward', 'Jewels Trails A', 'Jewels Trails B')
  unique_t <- unique(game$start)
  time_table <- matrix(0, nrow <- length(unique_t), ncol <- length(cat) * 3)
  for(i in 1 : length(unique_t)) {
    for(j in 1 : length(cat)) {
      temp <- subset(game, start == unique_t[i] & name == cat[j])
      time_table[i, 3 * (j - 1) + 1]= mean(temp$correct == 'TRUE')
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

# This function neeeds to be changed once we figure out how medication is coded
answer_to_score=function(survey){
  score=rep(NA,nrow(survey))
  score[survey$answer=='Not at all']=0
  score[survey$answer=='Several Times']=1
  score[survey$answer=='More than Half the Time']=2
  score[survey$answer=='Nearly All the Time']=3
  score[survey$name=='medication']=3-score[survey$name=='medication']
  score
}

#  same logic as game_reform
survey_reform=function(survey){
  survey$answer=as.numeric(as.character(survey$answer))
  cat = c('sleep','medication','social','psychosis','depression','anxiety')
  unique_t = unique(survey$start)
  time_table = matrix(0,nrow = length(unique_t), ncol=length(cat)*3)
  for(i in 1:length(unique_t)){
    for(j in 1:length(cat)){
      temp = subset(survey,start==unique_t[i] & name==cat[j])
      time_table[i,3*(j-1)+1]= mean(temp$answer)
      time_table[i,3*(j-1)+2]= mean(temp$time)/1000
      time_table[i,3*(j-1)+3]= nrow(temp)
    }
  }
  time_table=data.frame(time_table)
  column = c()
  for (i in 1:length(cat)){
    column=c(column,c(paste0(cat[i],'_score'),paste0(cat[i],'_time'),paste0(cat[i],'_row')))
  }
  colnames(time_table)=column
  time_table$date = unique_t
  row.names(time_table)=NULL
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
  name = c('Spatial Span Forward','Spatial Span Backward','Jewels Trails A','Jewels Trails B')
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


# also three options here: score, time
survey_plot=function(table,option){
  name = c('sleep','psychosis','medication','anxiety','social','depression')
  if (option=='score'){
    new_name = paste0(name,'_score')
    title_name = 'Score'
    y_name = "Score"
  }
  if (option=='time'){
    new_name = paste0(name,'_time')
    title_name = 'Mean Response Time'
    y_name = 'second'
  }
  sleep_dat <- make_df('date',new_name[1],y_name='value', source = table)
  sleep_dat$map='Sleep'
  psychosis_dat <- make_df('date',new_name[2],y_name='value', source = table)
  psychosis_dat$map='Psychosis'
  medication_dat <- make_df('date',new_name[3],y_name='value', source = table)
  medication_dat$map='Medication'
  anxiety_dat <- make_df('date',new_name[4],y_name='value', source = table)
  anxiety_dat$map='Anxiety'
  social_dat <- make_df('date',new_name[5],y_name='value', source = table)
  social_dat$map='Social'
  depression_dat <- make_df('date',new_name[6],y_name='value', source = table)
  depression_dat$map='Depression'
  # Create a GGPlot with each line chart.
  ggplot() + 
    geom_line(data = sleep_dat, 
              aes(x = date, y = value, color = map)) +
    geom_line(data = psychosis_dat, 
              aes(x = date, y = value, color = map)) +
    geom_line(data = medication_dat, 
              aes(x = date, y = value, color = map)) +
    geom_line(data = anxiety_dat, 
              aes(x = date, y = value, color = map)) +
    geom_line(data = social_dat, 
              aes(x = date, y = value, color = map)) +
    geom_line(data = depression_dat, 
              aes(x = date, y = value, color = map)) +
    geom_point(data = sleep_dat, 
              aes(x = date, y = value, color = map)) +
    geom_point(data = psychosis_dat, 
              aes(x = date, y = value, color = map)) +
    geom_point(data = medication_dat, 
              aes(x = date, y = value, color = map)) +
    geom_point(data = anxiety_dat, 
              aes(x = date, y = value, color = map)) +
    geom_point(data = social_dat, 
              aes(x = date, y = value, color = map)) +
    geom_point(data = depression_dat, 
              aes(x = date, y = value, color = map)) +
    labs(title=title_name, x="Date", y=y_name) +
    scale_colour_manual("", values = c("Sleep"="black","Psychosis"="red", "Medication"="blue",
                                       "Anxiety"="green","Social"="orange",'Depression'='purple'))
}


game_plot(patient_table(commandArgs()$result)$game,'sd')


