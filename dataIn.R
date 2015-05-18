###############################################################################
#
# Voting Patterns in European COTY.
# - code for blog post.
#
# Paul, 18th May, 2015

## Required packages and files
source("dataIn_func.R")
library(ggplot2)


## Load voting data

# load raw data; further formatting required as file formats non-uniform
voteDataList_Raw <- load_csvFiles2List( "data/importio" )

# format and save all data to dataframe
voteDataDf_allJuryMembers <- formatVoteData2_journo( voteDataList_Raw )



## Calculate Vote Quotient

# that is, the difference between:-
# - the overall vote that each car receives, and 
# - the vote that each JURY MEMBER gives to each car*

voteDataDf_journoQuotient <- calcVoteQuotient_journo( voteDataDf_allJuryMembers )



## Assign brand's home country for each model

# load lookup data ( car brand : home country)
carBrand_country_lu <- read.csv( "data/CarBrands_HomeCountries.csv", header=TRUE,
                            stringsAsFactors=FALSE)

# add to main dataframe the brand's home country, and whether the jury and brand
# country match (logical test)
voteDataDf_journoQuotient <- addBrandCountry( carBrand_country_lu,
                                               voteDataDf_journoQuotient )



## Boxplot 1 - Votes for cars from own country

# Find countries with home brands/manufacturers in the dataset
# i.e. ones with 'home' = True
brand_countries <- voteDataDf_journoQuotient[ voteDataDf_journoQuotient$homeJury, "brand_country"]
brand_countries <- unique(brand_countries)

# Only include votes from countries which have home brands/manufacturers
vote_quotient_BrandCountries <- voteDataDf_journoQuotient[
    voteDataDf_journoQuotient$jury_country %in%  brand_countries, ]

# Extreme vote variance - setting '0' at x centre of chart for clarity
minQuot <- min( voteDataDf_journoQuotient$quotient )
maxQuot <- max( voteDataDf_journoQuotient$quotient )
extremeVal <- max( abs(minQuot), maxQuot )

# Plot
set.seed(1234)
ggplot( vote_quotient_BrandCountries[vote_quotient_BrandCountries$homeJury, ],
                 aes( factor(jury_country), quotient ) ) +
    coord_flip()+
    geom_hline(xintercept=c(0), linetype="solid", size=2, colour="black") +
    geom_boxplot( outlier.shape=NA )+
    geom_hline(xintercept=c(0), linetype="solid", size=2,
               colour="grey", alpha=0.4 )+
    ylim( -extremeVal, extremeVal ) +
    geom_jitter(colour="red", alpha=0.6,
                size=2.5,
                position = position_jitter( width=0.05, height=0 ) ) +
    ylab("Variance from Overall Score") +
    xlab("") +
    theme(axis.text.y = element_text( size=16 ),
          axis.text.x  = element_text( size=16 ),
          axis.title.x = element_text( size=15 ) )


# Boxplot 2 - Votes from one country for all brand countries
# e.g. see the difference in how Ireland votes for different brand countries
theOneCountry <- "Ireland"
set.seed(1234)
ggplot2::ggplot( voteDataDf_journoQuotient[voteDataDf_journoQuotient$jury_country==theOneCountry, ],
                 aes( factor(brand_country), quotient ) ) +
    coord_flip()+
    geom_hline(xintercept=c(0), linetype="solid", size=2, colour="black") +
    geom_boxplot( outlier.shape=NA )+
    geom_hline(xintercept=c(0), linetype="solid", size=2,
               colour="grey", alpha=0.4 )+
    ylim( -extremeVal, extremeVal ) +
    geom_jitter(colour="slateblue3", alpha=0.8,
                size=3,
                position = position_jitter( width=0.05, height=0 ) )+
    ylab("Variance from Overall Score") +
    xlab("Brand Country") +
    theme(axis.text.y = element_text( size=16 ),
          axis.text.x  = element_text( size=16 ),
          axis.title = element_text( size=15 ) )


# Boxplot 3 - Facet plot for the four main brand countries.
# same idea as Boxplot 2 above but with separate plot for each country

# subset to only the large countries of interest
countriesOfInterest <- c( "France", "Germany", "Italy", "United Kingdom" )
voteDataDf_largeCountries <- voteDataDf_journoQuotient[voteDataDf_journoQuotient$jury_country %in% countriesOfInterest, ]

# create labels. eg. "French jury" is clearer than just "France"
voteDataDf_largeCountries$juryLabels <-
    mapply( juryCountryLabel, voteDataDf_largeCountries$jury_country )

set.seed(1234)
gg <- ggplot2::ggplot( voteDataDf_largeCountries,
                 aes( factor(brand_country), quotient ) ) +
    coord_flip()+
    geom_hline(xintercept=c(0), linetype="solid", size=2, colour="black") +
    geom_boxplot( outlier.shape=NA )+
    geom_hline(xintercept=c(0), linetype="solid", size=2,
               colour="grey", alpha=0.4 )+
    ylim( -extremeVal, extremeVal ) +
    geom_jitter(colour="slateblue3", alpha=0.6,
                size=2,
                position = position_jitter(width=0.05, height=0) )+
    ylab("Variance from Overall Score") +
    xlab("Brand Country") +
    theme(axis.text.y = element_text( size=12 ),
          axis.text.x  = element_text( size=16 ),
          axis.title = element_text( size=15 ) )
gg + facet_wrap( ~ juryLabels, ncol=2 ) +
    theme( strip.text.x = element_text( size=16 ) )
