if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)
my_username <- Sys.getenv( "my_username" )
my_password <- Sys.getenv( "my_password" )
library(lodown)
lodown( "seer" , output_dir = file.path( getwd() ) , 
	your_username = my_username , 
	your_password = my_password )
library(DBI)
seer_cat_df <- 
	file.path( 
		getwd() , 
		"incidence/yr1973_2014.seer9/LYMYLEUK.rds" 
	)

seer_df <- 
	transform( 
		seer_df , 
		
		survival_months = ifelse( srv_time_mon == 9999 , NA , as.numeric( srv_time_mon ) ) ,
		
		female = as.numeric( sex == 2 ) ,
		
		race_ethnicity =
			ifelse( race1v == 99 , "unknown" ,
			ifelse( nhiade > 0 , "hispanic" , 
			ifelse( race1v == 1 , "white non-hispanic" ,
			ifelse( race1v == 2 , "black non-hispanic , 
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
	
dbGetQuery( db , "SELECT COUNT(*) FROM " )

dbGetQuery( db ,
	"SELECT
		race_ethnicity ,
		COUNT(*) 
	FROM 
	GROUP BY race_ethnicity"
)
dbGetQuery( db , "SELECT AVG( survival_months ) FROM " )

dbGetQuery( db , 
	"SELECT 
		race_ethnicity , 
		AVG( survival_months ) AS mean_survival_months
	FROM 
	GROUP BY race_ethnicity" 
)
dbGetQuery( db , 
	"SELECT 
		marital_status_at_dx , 
		COUNT(*) / ( SELECT COUNT(*) FROM ) 
			AS share_marital_status_at_dx
	FROM 
	GROUP BY marital_status_at_dx" 
)
dbGetQuery( db , "SELECT SUM( survival_months ) FROM " )

dbGetQuery( db , 
	"SELECT 
		race_ethnicity , 
		SUM( survival_months ) AS sum_survival_months 
	FROM 
	GROUP BY race_ethnicity" 
)
RSQLite::initExtension( db )

dbGetQuery( db , 
	"SELECT 
		LOWER_QUARTILE( survival_months ) , 
		MEDIAN( survival_months ) , 
		UPPER_QUARTILE( survival_months ) 
	FROM " 
)

dbGetQuery( db , 
	"SELECT 
		race_ethnicity , 
		LOWER_QUARTILE( survival_months ) AS lower_quartile_survival_months , 
		MEDIAN( survival_months ) AS median_survival_months , 
		UPPER_QUARTILE( survival_months ) AS upper_quartile_survival_months
	FROM 
	GROUP BY race_ethnicity" 
)
dbGetQuery( db ,
	"SELECT
		AVG( survival_months )
	FROM 
	WHERE rept_src == 1"
)
RSQLite::initExtension( db )

dbGetQuery( db , 
	"SELECT 
		VARIANCE( survival_months ) , 
		STDEV( survival_months ) 
	FROM " 
)

dbGetQuery( db , 
	"SELECT 
		race_ethnicity , 
		VARIANCE( survival_months ) AS var_survival_months ,
		STDEV( survival_months ) AS stddev_survival_months
	FROM 
	GROUP BY race_ethnicity" 
)
seer_slim_df <- 
	dbGetQuery( db , 
		"SELECT 
			survival_months , 
			female ,
			marital_status_at_dx
		FROM " 
	)

t.test( survival_months ~ female , seer_slim_df )
this_table <-
	table( seer_slim_df[ , c( "female" , "marital_status_at_dx" ) ] )

chisq.test( this_table )
glm_result <- 
	glm( 
		survival_months ~ female + marital_status_at_dx , 
		data = seer_slim_df
	)

summary( glm_result )
library(dplyr)
library(dbplyr)
dplyr_db <- dplyr::src_sqlite( dbdir )
seer_tbl <- tbl( dplyr_db , '' )
seer_tbl %>%
	summarize( mean = mean( survival_months ) )

seer_tbl %>%
	group_by( race_ethnicity ) %>%
	summarize( mean = mean( survival_months ) )
