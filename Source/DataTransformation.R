library(jsonlite)


CountJSONArrayInFeature <- function(feature)
{
    return(ifelse(feature == "", 0, nrow(fromJSON(feature))))
}

CountMembersInCrewByJobType <- function(crew, job.type)
{
    if(crew == "")
    {
        return(0)
    }
    
    crew_dataset <- fromJSON(crew)
    
    return(length(crew_dataset[crew_dataset$job == job.type, "job"]))
}

ExtractDataFromJSON <- function(movie, movie.feature, subfeatures.to_exclude)
{
    names <- fromJSON(movie[, movie.feature])
    names.length <- nrow(names)
    
    names$movie_id <- rep(movie$id, names.length)
    names$revenue <- rep(movie$revenue, names.length)
    names$budget <- rep(movie$budget, names.length)
    
    if(!is.null(subfeatures.to_exclude))
    {
        names[, subfeatures.to_exclude] <- NULL
    }
    
    return(names)
}

AppendExtractedData <- function(dataset, feature, subfeatures.to_exclude = NULL)
{
    movies.feature <- data.frame()
    dataset.length <- nrow(dataset)
    for(i in seq(1:dataset.length))
    {
        movie <- dataset[i, ]
        if(movie[, feature] != "")
        {
            movies.feature <- rbind(movies.feature, ExtractDataFromJSON(movie, feature, subfeatures.to_exclude))
        }
    }
    
    return(movies.feature)
}

ExtractBestRevenueFromGenres <- function(dataset, movies.genres)
{
    genre.best_revenue <- c()
    dataset.length <- nrow(dataset)
    for(i in seq(1:dataset.length))
    {
        if(dataset[i, "genres"] != "")
        {
            genres <- fromJSON(dataset[i, "genres"])
            genres.mean_revenue <- lapply(genres$name, function(name) movies.genres[movies.genres$name == name, c("id", "mean_revenue")]) %>% bind_rows()
            genre.best_revenue <- c(genre.best_revenue, genres.mean_revenue$id[which.max(genres.mean_revenue$mean_revenue)])
        }
        else
        {
            genre.best_revenue <- c(genre.best_revenue, 0)
        }
    }
    
    return(genre.best_revenue)
}

GetScoreByProductionCompanyName <- function(dataset, production.companies)
{
    production.companies %>%
        group_by(name) %>%
        summarise(mean_revenue = if("revenue" %in% names(.)) mean(revenue) else 0,
                  mean_budget = mean(budget),
                  number_of_movies = n()) %>%
        mutate(score = mean_budget * number_of_movies / nrow(dataset)) %>%
        arrange(desc(score))
}

ExtractBestScoreFromProductionCompanies <- function(dataset, production.companies)
{
    production.companies.best_score <- c()
    dataset.length <- nrow(dataset)
    for(i in seq(1:dataset.length))
    {
        if(dataset[i, "production_companies"] != "")
        {
            production_companies <- fromJSON(dataset[i, "production_companies"])
            production.companies.score <- lapply(production_companies$name, 
                                                 function(name) production.companies[production.companies$name == name, c("name", "score")]) %>% bind_rows()
            production.companies.best_score <- c(production.companies.best_score, production.companies.score$score[which.max(production.companies.score$score)])
        }
        else
        {
            production.companies.best_score <- c(production.companies.best_score, 0)
        }
    }
    
    return(production.companies.best_score)
}