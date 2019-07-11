FixJSONStandardErrors <- function(dataset_column)
{
    ## Replace all double quotes by single quotes.
    dataset_column <- gsub("\"", "\'", dataset_column, fixed=TRUE)
    
    ## Rule 5: The key starts a new element of an array.
    dataset_column <- gsub("\\{\'", "{\"", dataset_column)
    
    ## Rule 6: The string value ends an element of an array.  
    dataset_column <- gsub("\'\\}", "\"}", dataset_column)
    
    ## Rule 4: Replace None by null.
    dataset_column <- gsub("None", "null", dataset_column, fixed=TRUE)
    
    ## Rules 3 and 4: Remove the backslashes, [] and #N/A.
    dataset_column <- gsub("\\[\\]|#N/A|\\\\", "", dataset_column)
    
    ## Rule 1 and 2: Replace single quotes ending the keys by double quotes followed by a colon, a space and its value (null, integer or string).
    dataset_column <- gsub("\': (null|[[:digit:]])", "\": \\1", dataset_column)
    dataset_column <- gsub("\': \'", "\": \"", dataset_column, fixed=TRUE)
    
    ## Rule 7: Mapping (key, value) is separated by a coma.
    dataset_column <- gsub("(null|[[:digit:]]), \'", "\\1, \"", dataset_column)
    dataset_column <- gsub("\', \'", "\", \"", dataset_column, fixed=TRUE)
}


PrintNumberOfInvalidValuesDetected <- function(dataset, features_to_validate)
{
    for(key in names(features_to_validate))
    {
        value <- features_to_validate[key]
        key_to_validate <- paste0("[{\"", value, "\": ")
        
        invalid_values <- dataset[dataset[, key] != "" & !startsWith(dataset[, key], key_to_validate), key]
        cat("Number of detected invalid values in the feature '", key, "': ", length(invalid_values == TRUE), "\n", sep = "")
    }
}