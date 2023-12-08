## server.R
library(dplyr)
library(ggplot2)
library(recommenderlab)
#library(DT)
library(data.table)
library(reshape2)

myurl = "https://liangfgithub.github.io/MovieData/"

# read in movies dataframe
movies_df = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies_df = strsplit(movies_df, split = "::", fixed = TRUE, useBytes = TRUE)
movies_df = matrix(unlist(movies_df), ncol = 3, byrow = TRUE)
movies_df = data.frame(movies_df, stringsAsFactors = FALSE)
colnames(movies_df) = c('MovieID', 'Title', 'Genres')
movies_df$MovieID = as.integer(movies_df$MovieID)
movies_df$Title = iconv(movies_df$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies_df$image_url = sapply(movies_df$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))
# read in user dataframe
users_df = read.csv(paste0(myurl, 'users.dat?raw=true'),
                    sep = ':', header = FALSE)
users_df = users_df[, -c(2,4,6,8)] # skip columns
colnames(users_df) = c('UserID', 'Gender', 'Age', 'Occupation', 'Zip-code')

# read in ratings dataframe
ratings_df = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                      sep = ':',
                      colClasses = c('integer', 'NULL'), 
                      header = FALSE)
colnames(ratings_df) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

# read in ratings matrix


get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

# Based on selected genre, chooses 10 popular recommendations
system_1_solver = function(genre)
{
  print(genre)
  
  # get movieID's of movies in that genre
  ids_in_genre = movies_df[grepl(genre, movies_df$Genres), "MovieID"]
  
  # If we have recommended for this genre already, load that saved data
  # instead of re-computing
  if (sum(saved_genre_recoms[[genre]]$MovieID) > 0)
  {
    ret = saved_genre_recoms[[genre]]
    print("reusing system1 genre recoms!")
  }
  else
  {
    ret = group_by(ratings_df, MovieID)
    ret = summarize(ret, ratings_per_movie = n(), ave_ratings = round(mean(Rating), dig=3))
    ret = inner_join(ret, movies_df, by = 'MovieID')
    ret = filter(ret, ratings_per_movie > 100)
    ret = filter(ret, MovieID %in% ids_in_genre)
    ret = top_n(ret, 10, ave_ratings)
    ret = select(ret, 'ave_ratings', 'MovieID')
    ret = arrange(ret, desc(-ave_ratings))  
    
    saved_genre_recoms[[genre]] <<- ret
  }
  ret
}

# TODO implement
system_2_solver = function(user_ratings)
{
  # create df for top 10 recommendation id's and calculated scores
  predictions = data.frame(ids = rep(0, 10), scores = rep(0, 10))
  
  # #-#-# dummy code, always returns first 10 movies
  predictions$ids = 1:10
  predictions$scores = (1:10)/10
  # #-#-#
  
  predictions
}

# Actual running server with event callbacks
shinyServer(function(input, output, session) {
  
  # show the books to be rated
  output$ratings = renderUI({
    num_rows = 20
    num_movies = 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies_df$image_url[(i - 1) * num_movies + j], height = 150)),
                 #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center", strong(movies_df$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies_df$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  genre_recom_df = eventReactive(input$genre_submit_button, {
    withBusyIndicatorServer("genre_submit_button", { # showing the busy indicator
      
      user_predicted_ids = system_1_solver(input$genre_selection)
      titles = inner_join(user_predicted_ids, movies_df, by="MovieID")
      recom_results = data.table(Rank = 1:10, 
                                 MovieID = user_predicted_ids$MovieID, 
                                 Title = titles$Title)
      recom_results
    })
  })
  
  # Calculate recommendations when the sbumbutton is clicked
  rating_recom_df = eventReactive(input$ratings_submit_button, {
    withBusyIndicatorServer("ratings_submit_button", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode = "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # MovieID's of only movies that the user rated
      value_list = reactiveValuesToList(input)
      # ratings of only the movies that the user rated
      user_ratings = get_user_ratings(value_list)
      
      predictions = system_2_solver(user_ratings)
      user_results = predictions$scores
      user_predicted_ids = predictions$ids
      recom_results = data.table(Rank = 1:10, 
                                     MovieID = movies_df$MovieID[user_predicted_ids], 
                                     Title = movies_df$Title[user_predicted_ids], 
                                     Predicted_rating =  user_results)
      recom_results
    }) # still busy
  }) # clicked on button
  
  # display the genre recommendations
  output$genre_results = renderUI({
    num_rows = 2
    num_movies = 5
    recom_result = genre_recom_df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies_df$image_url[movies_df$MovieID == recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies_df$Title[movies_df$MovieID == recom_result$MovieID[(i - 1) * num_movies + j]])
            )
            
        )        
      }))) # columns
    }) # rows
  }) # renderUI function
   
  # display the ratings recommendations
  output$ratings_results = renderUI({
    num_rows = 2
    num_movies = 5
    recom_result = rating_recom_df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies_df$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies_df$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
            )
            
        )        
      }))) # columns
    }) # rows
  }) # renderUI function
  
}) # server function


