###############################################################################
#
# Voting Patterns in European COTY - Functions used
#
# Paul, 18th May 2015.

## Required packages
library(reshape2)
library(dplyr)


## Load multiple CSV files to a list
load_csvFiles2List <- function( path ) { 
    # load csv data to list
    files <- dir( path, pattern = '\\.csv', full.names = TRUE )
    voteDataList <- lapply( files, read.csv, skip=1, header=TRUE)

    # name list elements with vote years
    years <- sub(".*?importio/(.*?).csv", "\\1", files )
    names( voteDataList ) <- years
    return( voteDataList )
}


## Format csv file data and combine into one dataframe
formatVoteData <- function( voteDataList_Raw ){
    
    # initialise overall df
    voteDataDf <- data.frame()
    
    for ( voteYear in names( voteDataList_Raw ) ){

        # extract country_value, and fill in blanks
        df <- voteDataList_Raw[[ voteYear ]]
        countryVec <- as.character( df[ , "country_value" ] )
        countryVec <- as.character( fillDownVector(countryVec) )
                
        # extract dataframe of scores
        # the relevant cols have header names ending with "_number", This is 
        # used to locate the relevant columns
        header <- names( df )
        scoreCols <- grep( "number", header, perl=TRUE )
        scoreDf <- df[ , scoreCols ]
        
        # combine data. include column for year of vote
        yearVec <- rep( voteYear, length( countryVec ) )
        realData <- cbind( jury_country=countryVec,
                           year=yearVec,
                           scoreDf,
                           stringsAsFactors = FALSE)

        # melt data to long data form, and save to overall dataframe
        realDataMelt <- melt( realData, id=c("jury_country", "year") )
        voteDataDf <- rbind( voteDataDf, realDataMelt )    
    }
    
    # format model name; remove "number" and add vote year
    modelNameRaw <- voteDataDf[ , 3 ]
    modelNameEdit <- sub("(.*?)_.*", "\\1", modelNameRaw )
    yearVec <- voteDataDf[ , 2 ]
    modelNameEdit <- paste0( modelNameEdit, "_", yearVec )
    
    # add edited model column and tidy headers
    voteDataDf[ , 3 ] <- modelNameEdit
    names( voteDataDf )[ 3:4 ] <- c( "car", "vote" ) 
    return( voteDataDf )
}


## Format csv file data and combine into one dataframe - Version 2 JOURNALISTS SEPARATE
formatVoteData2_journo <- function( voteDataList_Raw ){
    
    # initialise overall df
    voteDataDf <- data.frame()
    
    for ( voteYear in names( voteDataList_Raw ) ){
        
        # extract country_value, and fill in blanks
        df <- voteDataList_Raw[[ voteYear ]]
        countryVec <- as.character( df[ , "country_value" ] )
        countryVec <- as.character( fillDownVector(countryVec) )
        
        # extract journalist
        # problem as different headings used for different years
        if ( "name_value" %in% names(df)){
            journoVec <- as.character( df[ , "name_value" ] )            
        }
        else{
            journoVec <- as.character( df[ , "name_link._text" ] )
        }
        
        # extract dataframe of scores
        # the relevant cols have header names ending with "_number", This is 
        # used to locate the relevant columns
        header <- names( df )
        scoreCols <- grep( "number", header, perl=TRUE )
        scoreDf <- df[ , scoreCols ]
        
        # combine data. include column for year of vote
        yearVec <- rep( voteYear, length( countryVec ) )
        realData <- cbind( jury_country=countryVec,
                           journalist=journoVec,
                           year=yearVec,
                           scoreDf,
                           stringsAsFactors = FALSE)
        
        # melt data to long data form, and save to overall dataframe
        realDataMelt <- melt( realData, id=c("jury_country", "year", "journalist") )
        voteDataDf <- rbind( voteDataDf, realDataMelt )    
    }
    
    # format model name; remove "number" and add vote year
    modelNameRaw <- voteDataDf[ , 4 ]
    modelNameEdit <- sub("(.*?)_.*", "\\1", modelNameRaw )
    yearVec <- voteDataDf[ , 2 ]
    modelNameEdit <- paste0( modelNameEdit, "_", yearVec )
    
    # add edited model column and tidy headers
    voteDataDf[ , 4 ] <- modelNameEdit
    names( voteDataDf )[ 4:5 ] <- c( "car", "vote" ) 
    return( voteDataDf )
}


## Fill empty spaces down in vector using the last name found
fillDownVector <- function( inVec ){
    outVec <- inVec
    for ( cell in 1:length( inVec ) ){
        if ( inVec[ cell ] != "" ){
            country <- inVec[ cell ]
        }
        if ( inVec[ cell ] == "" ) {
            outVec[ cell ] <- country        
        }
    }
    return( outVec )
}


## Calculate Vote Quotient
calcVoteQuotient_journo <- function( inDf ){
    
#     # average votes per car from each country
#     votes_PerCarAndJuryCountry <- dplyr::summarise( group_by( inDf, jury_country, year, car ),
#                                                     vote_byCountry = mean( vote ) )
#     
    # average votes per car (overall)
    votes_PerCar <- dplyr::summarise( group_by( inDf, car ),
                                      vote_overall = mean( vote ) )
    
    # join tables
    vote_countryQuotient <- inner_join( inDf, votes_PerCar,
                                        by = "car" )
    
    # vote quotient
    quotient <- vote_countryQuotient$vote - vote_countryQuotient$vote_overall
    vote_countryQuotient <- cbind( vote_countryQuotient, quotient )
    rm( quotient, votes_PerCar )
    
    return( vote_countryQuotient )
}


## Add to main dataframe the brand's home country, and whether the jury and
## brand country match (logical)
addBrandCountry <- function( carBrand_country_lu, voteDataDf_countryQuotient ){
    
    # create lookup table (car model : home country)
    # (to be used to combine with main vote df)
    model_countryLu <- createModelCountryLU( carBrand_country_lu,
                                             voteDataDf_countryQuotient )

    # add to main dataframe
    voteDataDf_countryQuotient <- inner_join( voteDataDf_countryQuotient,
                                              model_countryLu,
                                              by = "car" )
    
    # add logical test to show if jury and brand country match i.e. a home jury
    homeJury <- voteDataDf_countryQuotient$jury_country ==
        voteDataDf_countryQuotient$brand_country
    voteDataDf_countryQuotient <- cbind( voteDataDf_countryQuotient,
                                         homeJury )
    
    return( voteDataDf_countryQuotient )
}


## Create lookup table matching each model with its brand's home counry
createModelCountryLU <- function( carBrand_country_lu, voteDataDf_countryQuotient){

    # all models in dataset
    carModels <- unique( voteDataDf_countryQuotient$car )
    
    # create lookup table (model : home country)
    
    model_countryLu <- list()
    for ( i in 1:length( carBrand_country_lu$brand_asData ) ){
        
        # brand (and home country) for this loop
        brand <- carBrand_country_lu$brand_asData[ i ]
        country <- carBrand_country_lu$home_country[ i ]
        
        # find all models (as per voting data) belonging to the brand
        index <- grep( paste0( "^", brand ), carModels )
        models <- carModels[ index ]
        
        # create model county lookup list
        for ( model in models ){
            model_countryLu[[model]] <- country       
        }
        
    }
    # convert to df and return
    model_countryLuVec <- do.call( rbind.data.frame, model_countryLu )
    model_countryLuDf <- data.frame( row.names(model_countryLuVec),
                                     model_countryLuVec,
                                     stringsAsFactors=FALSE)
    colnames( model_countryLuDf ) <- c( "car", "brand_country")
    # ensure all columns are not factors
    model_countryLuDf[] <- lapply( model_countryLuDf, as.character )
    return( model_countryLuDf )
}


## Create labels for jurys to aid clarity of plots
## e.g. "French Jury" to replace "France"
juryCountryLabel <- function( country ){
    
    # Create Country lookup ( name : adjective ) 
    country_labelLuDf <- data.frame(
    noun = c( "France", "Germany", "Italy", "United Kingdom" ),
    adj = c( "French", "German", "Italian", "UK" ) )
    
    # Lookup country adjective
    if ( country %in% country_labelLuDf$noun ) {
        countryAdj <- country_labelLuDf[ country_labelLuDf$noun == country, 2 ]        
    } else {
        countryAdj <- "CODE ERROR!"
    }
    
    # Return full label
    label <- paste( countryAdj, "Jury" )
    return( label )
}
