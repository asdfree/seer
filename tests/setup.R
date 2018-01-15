if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)
my_username <- Sys.getenv( "my_username" )
my_password <- Sys.getenv( "my_password" )
library(lodown)
lodown( "seer" , output_dir = file.path( getwd() ) , 
	your_username = my_username , 
	your_password = my_password )
available_files <-
	list.files( 
		file.path( getwd() ) , 
		recursive = TRUE , 
		full.names = TRUE 
	)

seer_df <- 
	readRDS( grep( "incidence(.*)LYMYLEUK" , available_files , value = TRUE ) )

seer_df <- 
	transform( 
		seer_df , 
		
		survival_months = ifelse( srv_time_mon == 9999 , NA , as.numeric( srv_time_mon ) ) ,
		
		female = as.numeric( sex == 2 ) ,
		
		race_ethnicity =
			ifelse( race1v == 99 , "unknown" ,
			ifelse( nhiade > 0 , "hispanic" , 
			ifelse( race1v == 1 , "white non-hispanic" ,
			ifelse( race1v == 2 , "black non-hispanic" , 
				"other non-hispanic" ) ) ) ) ,
		
		marital_status_at_dx =
			factor( 
				as.numeric( mar_stat ) , 
				levels = c( 1:6 , 9 ) ,
				labels =
					c(
						"single (never married)" ,
						"married" ,
						"separated" ,
						"divorced" ,
						"widowed" ,
						"unmarried or domestic partner or unregistered" ,
						"unknown"
					)
			)
	)
	
nrow( seer_df )

table( seer_df[ , "race_ethnicity" ] , useNA = "always" )
mean( seer_df[ , "survival_months" ] )

tapply(
	seer_df[ , "survival_months" ] ,
	seer_df[ , "race_ethnicity" ] ,
	mean 
)
prop.table( table( seer_df[ , "marital_status_at_dx" ] ) )

prop.table(
	table( seer_df[ , c( "marital_status_at_dx" , "race_ethnicity" ) ] ) ,
	margin = 2
)
sum( seer_df[ , "survival_months" ] )

tapply(
	seer_df[ , "survival_months" ] ,
	seer_df[ , "race_ethnicity" ] ,
	sum 
)
quantile( seer_df[ , "survival_months" ] , 0.5 )

tapply(
	seer_df[ , "survival_months" ] ,
	seer_df[ , "race_ethnicity" ] ,
	quantile ,
	0.5 
)
sub_seer_df <- subset( seer_df , rept_src == 1 )
mean( sub_seer_df[ , "survival_months" ] )
var( seer_df[ , "survival_months" ] )

tapply(
	seer_df[ , "survival_months" ] ,
	seer_df[ , "race_ethnicity" ] ,
	var 
)
t.test( survival_months ~ female , seer_df )
this_table <- table( seer_df[ , c( "female" , "marital_status_at_dx" ) ] )

chisq.test( this_table )
glm_result <- 
	glm( 
		survival_months ~ female + marital_status_at_dx , 
		data = seer_df
	)

summary( glm_result )
library(dplyr)
seer_tbl <- tbl_df( seer_df )
seer_tbl %>%
	summarize( mean = mean( survival_months ) )

seer_tbl %>%
	group_by( race_ethnicity ) %>%
	summarize( mean = mean( survival_months ) )
