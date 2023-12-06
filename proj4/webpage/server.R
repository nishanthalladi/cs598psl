## server.R

# Webpage
get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

# TODO implement
system_1_solver = function(genre)
{
  print(genre)
  # create df for top 10 recommendation id's and calculated scores
  predictions = data.frame(ids = rep(0, 10), scores = rep(0, 10))
  
  #-#-# dummy code, always returns first 10 movies
  predictions$ids = 1:10 + 1
  #-#-#
  
  predictions
}

# TODO implement
system_2_solver = function(user_ratings)
{
  # create df for top 10 recommendation id's and calculated scores
  predictions = data.frame(ids = rep(0, 10), scores = rep(0, 10))
  
  #-#-# dummy code, always returns first 10 movies
  predictions$ids = 1:10
  predictions$scores = (1:10)/10
  #-#-#
  
  predictions
}

# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

shinyServer(function(input, output, session) {
  
  # show the books to be rated
  output$ratings = renderUI({
    num_rows = 20
    num_movies = 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  genre_recom_df = eventReactive(input$genre_submit_button, {
    withBusyIndicatorServer("genre_submit", { # showing the busy indicator
      
      predictions = system_1_solver(input$genre)
      user_predicted_ids = predictions$ids
      recom_results = data.table(Rank = 1:10, 
                                 MovieID = movies$MovieID[user_predicted_ids], 
                                 Title = movies$Title[user_predicted_ids])
      recom_results
    })
  })
  
  # Calculate recommendations when the sbumbutton is clicked
  rating_recom_df = eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
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
                                     MovieID = movies$MovieID[user_predicted_ids], 
                                     Title = movies$Title[user_predicted_ids], 
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
                a(img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
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
                a(img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
            )
            
        )        
      }))) # columns
    }) # rows
  }) # renderUI function
  
}) # server function


