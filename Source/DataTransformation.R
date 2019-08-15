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
        summarise(mean_log_revenue = mean(log(revenue + 1)),
                  mean_revenue = mean(revenue),
                  number_of_movies = n())
}

CountCrewMembersByJobType <- function(crew, job.type)
{
    number_of_members <- 0
    if(crew != "" && !is.na(crew))
    {
        crew.dataset <- fromJSON(crew)
        number_of_members <- length(crew.dataset[crew.dataset$job == job.type, "job"])
    }
    
    return(number_of_members)
}

Crew.GroupByDepartment <- function(dataset)
{
    crew.dataset <- AppendExtractedData(dataset, feature = "crew")
    
    return(crew.dataset %>%
        group_by(department) %>%
        summarise(number_of_members = n(),
                  mean_revenue = mean(revenue)))
}

ExtractOriginalLanguageIDFromName <- function(name, original_languages)
{
    return(original_languages$id[original_languages$name == name])
}

ExtractDataFromJSON <- function(movie, movie.feature, subfeatures.to_exclude)
{
    names <- fromJSON(movie[, movie.feature])
    if(!is.null(subfeatures.to_exclude))
    {
        names[, subfeatures.to_exclude] <- NULL
    }
    
    names.length <- nrow(names)
    
    if(any(names(movie) == "revenue"))
    {
        names$revenue <- rep(movie$revenue, names.length)
    }
    names$budget <- rep(movie$budget, names.length)
    
    return(names)
}

AppendExtractedData <- function(dataset, feature, subfeatures.to_exclude = NULL)
{
    movies.feature <- data.frame()
    dataset.length <- nrow(dataset)
    for(id in seq(1:dataset.length))
    {
        movie <- dataset[id, ]
        if(movie[, feature] != "" && !is.na(movie[, feature]))
        {
            movies.feature <- rbind(movies.feature, ExtractDataFromJSON(movie, feature, subfeatures.to_exclude))
        }
    }
    
    return(movies.feature)
}


CalculateScoresOfFeature <- function(dataset, coefficients, train.length)
{
    return(dataset %>%
        group_by(name) %>%
        summarise(score = (n()*coefficients["(Intercept)"] + coefficients["train$budget"]*sum(as.numeric(budget))) / train.length))
}

ExtractMaxScoreFromFeature <- function(dataset, feature.scores, feature)
{
    best_score_id <- c()
    dataset.length <- nrow(dataset)
    for(i in seq(1:dataset.length))
    {
        if(dataset[i, feature] != "" && !is.na(dataset[i, feature]))
        {
            features.extracted <- fromJSON(dataset[i, feature])
            feature.score_id <- lapply(features.extracted$name, function(name) feature.scores[feature.scores$name == name, c("id", "score")]) %>% bind_rows()
            best_score_id <- c(best_score_id, feature.score_id$id[which.max(feature.score_id$score)])
        }
        else
        {
            best_score_id <- c(best_score_id, 0)
        }
    }
    
    return(best_score_id)
}