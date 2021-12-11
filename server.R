## server.R

# load functions
# source('functions/cf_algorithm.R') # collaborative filtering
# source('functions/similarity_measures.R') # similarity measures

# define functions
get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                    Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

myurl = "https://liangfgithub.github.io/MovieData/"

prev_time <- Sys.time()

# read ratings data
ratings = read.csv('data/ratings.dat',
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
ratings$Timestamp = NULL

#print(paste0("ratings.dat took ", Sys.time()-prev_time))
prev_time = Sys.time()

# read movies data
movies = readLines('data/movies.dat')
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")
# extract year
movies$Year = as.numeric(unlist(lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, function(x) paste0(small_image_url, x, '.jpg?raw=true'))

#print(paste0("movies.dat took ", Sys.time()-prev_time))
prev_time = Sys.time()

# read users data
users = read.csv('data/users.dat', sep = ':', header = FALSE)
users = users[, -c(2,4,6,8)] # skip columns
colnames(users) = c('UserID', 'Gender', 'Age', 'Occupation', 'Zip-code')

#print(paste0("users.dat took ", Sys.time()-prev_time))
prev_time = Sys.time()

#use the whole ratings matrix to define Rmat
train = ratings

#Sparse Matrix
i = paste0('u', train$UserID)
j = paste0('m', train$MovieID)
x = train$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)
Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(Rmat) = levels(tmp$i)
colnames(Rmat) = levels(tmp$j)
Rmat = new('realRatingMatrix', data = Rmat)

#print(paste0("users.dat took ", Sys.time()-prev_time))
prev_time = Sys.time()

# UBCF Recommender Model

rec_UBCF = Recommender(Rmat, method = "UBCF",
                       parameter = list(normalize = 'Z-score', 
                                        method = 'Cosine', 
                                        nn = 25))
print(paste0("rec_UBCF took ", Sys.time()-prev_time))
prev_time = Sys.time()

shinyServer(function(input, output, session) {
  
  # Calculate top-N by Genre
  top_df <- eventReactive(input$showTopBtn, {
    withBusyIndicatorServer("showTopBtn", { # showing the busy indicator
      
      # get the user's selected genre
      genre_selected = c(input$genre)

      genres = as.data.frame(movies$Genres, stringsAsFactors=FALSE)
      tmp = as.data.frame(tstrsplit(genres[,1], '[|]',
                                    type.convert=TRUE),
                          stringsAsFactors=FALSE)
      genre_movies = movies[which(tmp[,1] == genre_selected |
                                    tmp[,2] == genre_selected |
                                    tmp[,3] == genre_selected |
                                    tmp[,4] == genre_selected |
                                    tmp[,5] == genre_selected |
                                    tmp[,6] == genre_selected ),]
      rating_genre = ratings[which(ratings[,"MovieID"] %in% genre_movies$MovieID),]
      
      #Find the movies which are rated by most users as the  popular movie.
      popular_movies  = rating_genre %>%  group_by(MovieID) %>% 
        summarize(ratings_per_movie = n(), ave_ratings = mean(Rating)) %>%
        inner_join(genre_movies, by = 'MovieID')
      
      popular_movies = popular_movies %>% arrange(desc(ratings_per_movie))
      
      Top_popular_movies = popular_movies[1:10,]
      #print(Top_popular_movies)
      recom_results <- data.table(Rank = 1:10,
                                  MovieID = Top_popular_movies$MovieID,
                                  Title = Top_popular_movies$Title,
                                  Predicted_rating =  Top_popular_movies$ave_ratings,
                                  image_url = Top_popular_movies$image_url)
      
    }) # still busy
    
  }) # clicked on button
  
  # display the top Movies by Genre
  output$topMovieResults <- renderUI({
    num_rows <- 2
    num_movies <- 5
    top_movie_result <- top_df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = top_movie_result$image_url[(i - 1) * num_movies + j], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(top_movie_result$Title[(i - 1) * num_movies + j])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
  
  # show the movies to be rated
  output$ratings <- renderUI({
      num_rows <- 20
      num_movies <- 5 # movies per row
      
      lapply(1:num_rows, function(i) {
        list(fluidRow(lapply(1:num_movies, function(j) {
          list(box(width = 2,
                   div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                   div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                   div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
        })))
      })
  })
  
  # Calculate recommendations when the submit button is clicked
  recomm_df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
        # hide the rating container
        useShinyjs()
        jsCode <- "document.querySelector('[data-widget=collapse]').click();"
        runjs(jsCode)
        
        # Handle NewUser Ratings
        value_list <- reactiveValuesToList(input)
        user_ratings <- get_user_ratings(value_list)
        
        #print(user_ratings)
        
        movieIDs = colnames(Rmat)
        n.item = ncol(Rmat)

        new.ratings = rep(NA, n.item)
        for(mr in seq_len(nrow(user_ratings))) {
            new.ratings[which(movies$MovieID == user_ratings$MovieID[mr])] = user_ratings$Rating[mr]
        }

        new.user = matrix(new.ratings, 
                          nrow=1, ncol=n.item,
                          dimnames = list(
                            user=paste('vsc5'),
                            item=movieIDs
                          ))
        new.Rmat = as(new.user, 'realRatingMatrix')
        
        # new user predictions
        pred_UBCF = predict(rec_UBCF, new.Rmat, n = 10, type = 'topNList')
        matched_movieIDs = getList(pred_UBCF)
        
        matched_movies = movies[which(colnames(Rmat) %in% matched_movieIDs[[1]]), ]
        
        recom_results <- data.table(Rank = 1:10, 
                                    MovieID = matched_movies$MovieID, 
                                    Title = matched_movies$Title,
                                    Predicted_rating = ratings[which(colnames(Rmat) %in% matched_movieIDs[[1]]), ]$Rating)
        
    }) # still busy
    
  }) # clicked on button
  

  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- recomm_df()
    
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