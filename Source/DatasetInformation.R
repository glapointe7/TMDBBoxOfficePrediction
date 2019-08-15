PrintDatasetInformation <- function(dataset)
{
    dataset.number_of_rows <- nrow(dataset)
    dataset.number_of_columns <- ncol(dataset)
    
    cat("Number of rows: ", dataset.number_of_rows, "\n")
    cat("Number of columns: ", dataset.number_of_columns, "\n\n")
    
    dataset.number_of_NA_and_empty <- data.frame(Type = sapply(dataset, class))
    dataset.number_of_NA_and_empty <- cbind(dataset.number_of_NA_and_empty, data.frame(Number_of_NA = colSums(is.na(dataset))))
    dataset.number_of_NA_and_empty <- cbind(dataset.number_of_NA_and_empty, data.frame(Number_of_empty = colSums(dataset == "")))
    
    knitr::kable(dataset.number_of_NA_and_empty)
}

