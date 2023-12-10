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

# read in S from CSV
S_post_step3_ = read.csv("S_post_step3.csv")
S = S_post_step3_[,-1]
rownames(S) <- S_post_step3_[,1]

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
    if (nrow(ret) < 10) {
      generic_recommendations = arrange(select(ratings_df, 'Rating', 'MovieID'), desc(Rating))[1:10,]
      ret = rbind(ret, generic_recommendations)
    }
    ret = top_n(ret, 10, ave_ratings)
    ret = select(ret, 'ave_ratings', 'MovieID')
    ret = arrange(ret, desc(-ave_ratings))  
    
    saved_genre_recoms[[genre]] <<- ret
  }
  ret
}

myIBCF = function(w) {
  recom_rankings = apply(S, 1, function(x){sum(x * w, na.rm=TRUE) / sum( (x * !is.na(w)), na.rm=TRUE)} )
  for (i in 1:length(recom_rankings))
  {
    if (!is.na(w[i]))
    {
      recom_rankings[i] = NA
    }
  }
  recom_rankings
}

process_user_rating_output = function(user_ratings)
{
  # Pull a single sample user from ratings matrix and set all values to NA
  ratings_mat_sample = read.csv("ratings_matrix.csv", nrows = 1)
  ratings_mat_sample[!is.na(ratings_mat_sample)] = NA

  # need to remove strange NA movie id
  temp = user_ratings[-1,]

  movie_ids = temp$MovieID
  for (id in 1:length(movie_ids))
  {
    text_id = paste("m", as.character(movie_ids[id]), sep="")
    ratings_mat_sample[text_id] = temp$Rating[id]
  }
  # convert to numeric vector to save processing time by like 30,000%
  as.numeric(ratings_mat_sample)
}

system_2_solver = function(user_ratings)
{
  processed_user_ratings = process_user_rating_output(user_ratings)
  # getting this far
  returned_rankings = myIBCF(processed_user_ratings)
  non_na_ranks = returned_rankings[!is.na(returned_rankings)]
  
  # If less than 10 recommendations, put some other movies in
  if (length(non_na_ranks) < 10) {
    nrank = 10 - length(non_na_ranks)
    generic_recommendations = arrange(select(ratings_df, 'Rating', 'MovieID'), desc(Rating))[1:nrank,]
    vector_generics = rep(1, nrank)#select(generic_recommendations, 'Rating')[['Rating']]
    vec_names = select(generic_recommendations, 'MovieID')[['MovieID']]
    for (id in 1:length(vec_names))
    {
      vec_names[id] = paste("m", as.character(vec_names[id]), sep="")
    }
    names(vector_generics) = vec_names
    full_ranks = c(non_na_ranks, vector_generics)
    print("less than 10 recoms, injecting...")
  }
  else
  {
    full_ranks = non_na_ranks
  }
  
  # format results as a dataframe with MovieIDs and Scores
  return_df = data.frame(
    ids = names(head(full_ranks[order(-unlist(full_ranks))], n=10)),
    scores = head(full_ranks[order(-unlist(full_ranks))], n=10)
  )
  return_df
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
      
      # ratings and movieID of only the movies that the user rated
      user_ratings = get_user_ratings(value_list)

      predictions = system_2_solver(user_ratings)
      user_results = predictions$scores
      # convert string m123 format back to numeric 123 format for movie id's
      user_predicted_ids = as.numeric(substring(predictions$ids, 2))
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


