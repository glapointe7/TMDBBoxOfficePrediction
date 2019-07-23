library(jsonlite)


CountJSONArrayInFeature <- function(feature)
{
    return(ifelse(feature == "", 0, nrow(fromJSON(feature))))
}

HasTheFeature <- function(feature)
{
    return(ifelse(feature == "", 0, 1))
}

Dataset.GroupByFeature <- function(dataset, feature)
{
    dataset %>%
        group_by_at(feature) %>% 
        summarise(mean_revenue = mean(revenue),
                  number_of_movies = n())
}

CountMembersInCrewByJobType <- function(crew, job.type)
{
    number_of_members <- 0
    if(crew != "" && !is.na(crew))
    {
        crew_dataset <- fromJSON(crew)
        number_of_members <- length(crew_dataset[crew_dataset$job == job.type, "job"])
    }
    
    return(number_of_members)
}

ExtractOriginalLanguageIDFromName <- function(name, original_languages)
{
    return(original_languages$id[original_languages$name == name])
}

ExtractDataFromJSON <- function(movie, movie.feature, subfeatures.to_exclude)
{
    names <- fromJSON(movie[, movie.feature])
    names.length <- nrow(names)
    
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
        if(movie[, feature] != "" && !is.na(movie[, feature]))
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
        if(dataset[i, "genres"] != "" && !is.na(dataset[i, "genres"]))
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
        if(dataset[i, "production_companies"] != "" && !is.na(dataset[i, "production_companies"]))
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