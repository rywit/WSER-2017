
library( rjson )
library( ggplot2 )
library( dplyr )
library( reshape2 )
library( Hmisc )
library( tidyr )

load.raw.data <- function() {

  start <- build.df( "start.json", "Start", 0 )
  lyon.ridge <- build.df( "lyon_ridge.json", "Lyon Ridge", 10.3 )
  red.star.ridge <- build.df( "red_star_ridge.json", "Red Star Ridge", 15.8 )
  duncan.canyon <- build.df( "duncan_canyon.json", "Duncan Canyon", 24.4 )
  robinson.flat <- build.df( "robinson_flat.json", "Robinson Flat", 30.3 )
  millers.defeat <- build.df( "millers_defeat.json", "Miller's Defeat", 34.4 )
  dusty.corners <- build.df( "dusty_corners.json", "Dusty Corners", 38 )
  last.chance <- build.df( "last_chance.json", "Last Chance", 43.3 )
  devils.thumb <- build.df( "devils_thumb.json", "Devil's Thumb", 47.8 )
  eldorado.creek <- build.df( "eldorado_creek.json", "El Dorado Creek", 52.9 )
  michigan.bluff <- build.df( "michigan_bluff.json", "Michigan Bluff", 55.7 )
  foresthill <- build.df( "foresthill.json", "Foresthill", 62 )
  peachstone <- build.df( "peachstone.json", "Peachstone", 70.7 )
  rucky.chucky <- build.df( "rucky_chucky.json", "Rucky Chucky", 78 )
  green.gate <- build.df( "green_gate.json", "Green Gate", 79.8 )
  auburn.lake.trails <- build.df( "auburn_lake_trails.json", "Auburn Lake Trails", 85.2 )
  quarry.road <- build.df( "quarry_road.json", "Quarry Road", 90.7 )
  pointed.rocks <- build.df( "pointed_rocks.json", "Pointed Rocks", 94.3 )
  robie.point <- build.df( "robie_point.json", "Robie Point", 98.9 )
  finish <- build.df( "finish.json", "Finish Line", 100.2 )
  
  wser <- rbind( start, lyon.ridge, red.star.ridge, duncan.canyon, robinson.flat, millers.defeat, dusty.corners,
                 last.chance, devils.thumb, eldorado.creek, michigan.bluff, foresthill, peachstone,
                 rucky.chucky, green.gate, auburn.lake.trails, quarry.road, pointed.rocks, robie.point,
                 finish )
  
  wser <- assign.finish( wser )

  wser <- assign.pace( wser )
  
  return( wser )  
}

build.df <- function( file.name, aid.name, distance ) {
  
  raw <- fromJSON( file = file.name )
  
  df <- data.frame( matrix( unlist( raw$data ), ncol = 10, byrow = T ), stringsAsFactors = F )
  
  ## Add column names
  colnames( df ) <- c( "position", "bib", "name", "display_name", "gender", "age", "time_in", "time_out", "elapsed", "status" )
  
  df <- select( df, -name )
  
  ## Add aid stateion name and distance
  df$aid_name <- aid.name
  df$aid_distance <- distance
  
  ## Turn elapsed time to string from factor
  df$position <- as.integer( df$position )
  
  ## Parse elapsed time into numeric
  df$elapsed.hrs <- sapply( df$elapsed, to.hours )
  
  return( df )
}

to.hours <- function( str ) {
  
  if ( str == "--:--" ) {
    return( 0.0 )
  } 
  
  tt <- as.numeric( unlist( strsplit( str, ":" ) ) )
  tt[ 1 ] + tt[ 2 ] / 60 + tt[ 3 ] / 3600
}

assign.finish <- function( data ) {

  ## Status "On Course" at the finish means DNF
  data[ data$aid_name == "Finish Line" & data$status == "On Course", ]$status <- "Finished"
  
  ## Assign finish status to all rows
  finishers <- data %>%
    filter( status == "Finished" ) %>%
    group_by( gender ) %>%
    arrange( elapsed.hrs ) %>%
    mutate( gender.finish.pos = row_number() ) %>%
    ungroup() %>%
    select( bib, gender, gender.finish.pos )

  num.finishers <- finishers %>%
    group_by( gender ) %>%
    summarise( last.finisher = as.integer( max( gender.finish.pos ) ) )
  
  dnfs <- data %>%
    filter( status == "DNF" ) %>%
    group_by( gender ) %>%
    arrange( desc( aid_distance ), elapsed.hrs ) %>%
    inner_join( num.finishers, by = "gender" ) %>%
    mutate( gender.dnf.pos = row_number() + last.finisher ) %>%
    ungroup() %>%
    select( bib, gender.dnf.pos )
      
  ## Assign gender aid station position
  data <- data %>%
    arrange( elapsed.hrs ) %>%
    group_by( gender, aid_name ) %>%
    mutate( gender.pos = row_number() ) %>%
    ungroup() %>%
    left_join( finishers, by = c( "bib", "gender" ) ) %>%
    left_join( dnfs, by = "bib" ) %>%
    mutate( gender.final.pos = coalesce( gender.finish.pos, gender.dnf.pos ) ) %>%
    as.data.frame()

  ## Set position to NA at start line
  data[ data$aid_name == "Start", "gender.pos" ] <- NA
  
  ## Set final finish status
  data %>%
    filter( aid_name == "Finish Line" ) %>%
    mutate( gender.final.status = get.final.status( gender.final.pos, elapsed.hrs ) ) %>%
    select( bib, gender.final.status ) %>%
    right_join( data, by = "bib" ) %>%
    mutate( gender.final.status = coalesce( gender.final.status, "DNF" ) ) %>%
    mutate( silver.buckle = ifelse( gender.final.status %in% c( "Top 10", "Sub-24 Hours" ), T, F ) ) %>%
    mutate( buckle = ifelse( gender.final.status == "DNF", F, T ) ) %>%
    mutate( gender.final.status = factor( gender.final.status,
                                          ordered = T,
                                          levels = c( "Top 10", "Sub-24 Hours", "24+ Hours", "DNF" ) ) )
}

assign.pace <- function( data ) {
  
  data %>%
    arrange( aid_distance ) %>%
    group_by( bib ) %>%
    mutate( seg.dist = c( NA, diff( aid_distance ) ) ) %>%
    mutate( seg.time = c( NA, diff( elapsed.hrs ) ) ) %>%
    mutate( seg.pace = 60 * seg.time / seg.dist ) %>%
    ungroup() %>%
    as.data.frame()
}

get.final.status <- function( gender.final.pos, elapsed.hrs ) {

  ifelse( gender.final.pos <= 10, "Top 10",
          ifelse( elapsed.hrs <= 24, "Sub-24 Hours",
                  ifelse( elapsed.hrs <= 30, "24+ Hours", "DNF" ) ) )
}

## Build sub-24 model
calc.24hr.cutoff <- function( all.data, aid.name ) {
  
  data <- all.data %>% filter( aid_name == aid.name )
  
  model <- glm( silver.buckle ~ elapsed.hrs, data = data, family = "binomial" )
  
  data$pred <- predict( model, type = "response" )
  
  as.numeric( -model$coefficients[ 1 ] / model$coefficients[ 2 ] )
}

assign.prob <- function( elapsed.hrs, silver.buckle ) {

  data <- data.frame( x = elapsed.hrs, y = as.numeric( silver.buckle ) )
  
  model <- glm( y ~ x, data = data, family = "binomial" )
  
  as.vector( predict( model, data, type = "response" ) )
}

gender_labeller <- function(variable,value){
  ifelse( value == "M", "Men", "Women" )
}

buckle_labeller <- function(variable,value){
  print( value )
  ifelse( value == T, "Silver Buckle", "No Silver Buckle" )
}

wser <- load.raw.data()

aid.stations <- wser %>%
  select( aid_name, aid_distance ) %>%
  unique() %>%
  arrange( aid_distance ) %>%
  mutate( aid_num = row_number() ) %>%
  mutate( aid_desc = sprintf( "%.1f %s", aid_distance, aid_name ) ) %>%
  mutate( aid_desc = factor( aid_desc, levels = aid_desc, ordered = T ) )

## Assign model times
res.24 <- lapply( aid.stations$aid_name, function( aid_name ) calc.24hr.cutoff( wser, aid_name ) )
aid.stations$cutoff.24 <- coalesce( unlist( res.24 ), 0 )

# Add cutoff data
wser <- wser %>%
  inner_join( select( aid.stations, aid_name, aid_desc, cutoff.24 ), by = "aid_name" ) %>%
  group_by( aid_name ) %>%
  mutate( silver.prob = assign.prob( elapsed.hrs, silver.buckle ) ) %>%
  mutate( silver.prob = ifelse( is.na( silver.prob ), 0.5, silver.prob ) ) %>%
  ungroup()

wser[ wser$aid_name == "Start", "silver.prob" ] <- 0.5

aid.dist <- unique( aid.stations$aid_distance )

women <- filter( wser, gender == "F" )
men <- filter( wser, gender == "M" )

ggplot( wser, aes( x = aid_distance, y = elapsed.hrs, group = bib, colour = gender.final.status ) ) +
  geom_vline( xintercept = aid.dist, linetype = "dashed", alpha = 0.5 ) +
  geom_line() +
  facet_wrap( ~ gender, ncol = 1, labeller = gender_labeller ) +
  xlab( "Distance (miles)" ) +
  ylab( "Elapsed Time (hours)" ) +
  ggtitle( "Cumulative Elapsed Time" ) +
  scale_color_discrete( name = "Finishing Status" )

ggplot() +
  geom_vline( xintercept = aid.dist, linetype = "dashed", alpha = 0.5 ) +
  geom_line( data = men, aes( x = aid_distance, y = elapsed.hrs, group = bib, colour = gender.final.status ) ) +
  geom_line( data = aid.stations, aes( x = aid_distance, y = cutoff.24 ), size = 1 ) +
  facet_wrap( ~ silver.buckle, ncol = 1, labeller = buckle_labeller ) +
  xlab( "Distance (miles)" ) +
  ylab( "Elapsed Time (hours)" ) +
  ggtitle( "Cumulative Elapsed Time (with 24-Hour Pace)" ) +
  scale_color_discrete( name = "Finishing Status" )

## Plot Foresthill model
foresthill <- wser %>%
  filter( aid_name == "Foresthill" ) %>%
  select( elapsed.hrs, silver.buckle, gender.final.status )

ggplot( foresthill, aes( x = elapsed.hrs, y = as.numeric( silver.buckle ) ) ) +
  geom_jitter( height = 0.02 ) +
  geom_smooth( method="glm", method.args=list( family="binomial" ) ) +
  xlab( "Elapsed Time (hours)" ) +
  ylab( "Probability of Silver Buckle" ) +
  ggtitle( "Silver Buckle Probability (Foresthill Aid Station)" )
  
## Probability evolution
ggplot( wser, aes( x = aid_distance, y = silver.prob, group = bib, colour = gender.final.status ) ) +
  geom_vline( xintercept = aid.dist, linetype = "dashed", alpha = 0.25 ) +
  geom_hline( yintercept = c( 0, 1 ), colour = "black", alpha = 0.5, size = 1.5 ) +
  geom_hline( yintercept = 0.5, colour = "black", alpha = 0.25, size = 1 ) +
  geom_line() +
  facet_wrap( ~ gender.final.status ) +
  xlab( "Distance (miles)" ) +
  ylab( "Probability of Silver Buckle" ) +
  ggtitle( "Silver Buckle Probability by Distance" ) +
  scale_color_discrete(name="Finishing Status")

ggplot( filter( wser, silver.buckle == T ), aes( x = aid_distance, y = silver.prob, group = bib ) ) +
  geom_vline( xintercept = aid.dist, linetype = "dashed", alpha = 0.25 ) +
  geom_hline( yintercept = c( 0, 1 ), colour = "black", alpha = 0.5, size = 1.5 ) +
  geom_hline( yintercept = 0.5, colour = "black", alpha = 0.25, size = 1 ) +
  geom_line( colour = "#AAAAAA" ) +
  geom_line( data = filter( wser, display_name == "Andrew Stevens" ), colour = "red", size = 1 ) +
  xlab( "Distance (miles)" ) +
  ylab( "Probability of Silver Buckle" ) +
  ggtitle( "Silver Buckle Probability by Distance (Andrew Stevens)" )


## How accurate is our prediction
conf <- wser %>%
  filter( aid_distance > 0 ) %>%
  mutate( pred.correct = ifelse( ( elapsed.hrs < cutoff.24 ) == silver.buckle, 1, 0 ) ) %>%
  group_by( aid_name, aid_distance, aid_desc ) %>%
  summarise( num.correct = sum( pred.correct ), tot = n(), prop = 100 * num.correct / n() )

ggplot( conf, aes( x = aid_distance, y = prop ) ) +
  geom_vline( xintercept = aid.dist, linetype = "dashed", alpha = 0.3 ) +
  geom_text( aes( label = aid_desc ), nudge_y = -0.2, nudge_x = 0.5, size = 3, hjust = 0, alpha = 0.75, check_overlap = F, angle = -70 ) +
  geom_point() +
  geom_line() +
  xlab( "Distance (miles)" ) +
  ylab( "Accuracy (%)" ) +
  ggtitle( "Silver Buckle Model Accuracy" ) +
  ylim( c( 82, 100 ) )

## Scatters by aid station
finishes <- wser %>%
  filter( aid_name == "Finish Line" ) %>%
  mutate( finish.time = elapsed.hrs ) %>%
  select( bib, finish.time ) %>%
  right_join( wser, by = "bib" ) %>%
  select( aid_name, aid_distance, elapsed.hrs, finish.time, silver.buckle, gender.final.status, gender ) %>%
  mutate( finish.time = coalesce( finish.time, 30 ) ) %>%
  inner_join( aid.stations, by = c( "aid_name", "aid_distance" ) ) %>%
  filter( aid_distance > 0 )

men <- finishes %>% filter( gender == "M" )
women <- finishes %>% filter( gender == "F" )

ggplot( finishes, aes( x = elapsed.hrs, y = finish.time, colour = gender.final.status ) ) +
  geom_hline( yintercept = 24, alpha = 0.5 ) +
  geom_vline( aes( xintercept = cutoff.24 ) ) +
  geom_point() +
  facet_wrap( ~ aid_desc, scales = "free" ) +
  xlab( "Elapsed Time (hours)" ) +
  ylab( "Finish Time (hours)" ) +
  ggtitle( "Aid Station Pace Chart" ) +
  scale_color_discrete(name="Finishing Status")

## Pace diff chart

res <- aid.stations %>%
  mutate( cum.pace.24 = cutoff.24 * 60 / aid_distance ) %>%
  inner_join( wser, by = c( "aid_name", "aid_distance" ) ) %>%
  mutate( cum.pace = elapsed.hrs * 60 / aid_distance ) %>%
  mutate( pace.diff = cum.pace - cum.pace.24 ) %>%
  select( gender, aid_distance, bib, gender.final.status, pace.diff )

men <- res %>% filter( gender == "M" )
women <- res %>% filter( gender == "F" )

ggplot( men, aes( x = aid_distance, y = pace.diff, group = bib, colour = gender.final.status ) ) +
  geom_hline( yintercept = 0, colour = "black" ) +
  geom_vline( xintercept = aid.stations$aid_distance, linetype = "dashed", alpha = 0.2 ) +
  geom_line() +
  facet_wrap( ~ gender.final.status, scales = "free" ) +
  scale_color_discrete( name = "Finishing Status" ) +
  xlab( "Distance (miles)" ) +
  ylab( "Runner Pace vs Empirical Cutoff Pace (min/mile)" ) +
  ggtitle( "Cumulative Pace vs Empirical 24-Hour Pace (Men)" )

ggplot( women, aes( x = aid_distance, y = pace.diff, group = bib, colour = gender.final.status ) ) +
  geom_hline( yintercept = 0, colour = "black" ) +
  geom_vline( xintercept = aid.stations$aid_distance, linetype = "dashed", alpha = 0.2 ) +
  geom_line() +
  facet_wrap( ~ gender.final.status, scales = "free" ) +
  scale_color_discrete( name = "Finishing Status" ) +
  xlab( "Distance (miles)" ) +
  ylab( "Runner Pace vs Empirical Cutoff Pace (min/mile)" ) +
  ggtitle( "Cumulative Pace vs Empirical 24-Hour Pace (Women)" )

## Compare to official 24-hour pace
official.24.pace <- c( 0, 2.167,3.333,5.000,6.500,7.250,7.917,8.917,10.250,11.333,12.333,
                      13.750,15.750,17.667,18.333,19.833,21.167,22.333,23.667,24.000 )

pace.diff <- aid.stations %>%
  mutate( official.hrs = official.24.pace ) %>%
  mutate( diff = cutoff.24 - official.hrs ) %>%
  rbind( data.frame( aid_name = "Start", aid_distance = 0, aid_num = 0,
                     aid_desc = "0.0 Start", cutoff.24 = 0,
                     official.hrs = 0, diff = 0 ) )

ggplot( pace.diff, aes( x = aid_distance, y = diff * 60 ) ) +
  geom_hline( yintercept = 0, alpha = 0.5 ) +
  geom_text( aes( label = aid_desc ), nudge_y = 1, nudge_x = 0, size = 3, hjust = 0, alpha = 0.75, check_overlap = F, angle = 90, colour = "#003182" ) +
  geom_line( alpha = 0.5 ) +
  geom_point() +
  ylim( c( -11, 47 ) ) +
  xlab( "Distance (miles)" ) +
  ylab( "Empirical Cutoff - Official Cutoff (minutes)" ) +
  ggtitle( "Official 24-Hour Cutoff vs Empirical" )

## Segment pace
seg.pace <- aid.stations %>%
  arrange( aid_distance ) %>%
  mutate( seg.dist = c( NA, diff( aid_distance ) ) ) %>%
  mutate( seg.time = c( NA, diff( cutoff.24 ) ) ) %>%
  mutate( seg.pace = 60 * seg.time / seg.dist ) %>%
  mutate( seg.pace.min = floor( seg.pace ), seg.pace.sec = 60 * ( seg.pace - seg.pace.min ) )

idx <- seq( 2, nrow( seg.pace ) )
seg.names <- sapply( idx, function( i ) sprintf( "%s to %s (%.0f:%02.0f)", seg.pace$aid_name[ i-1 ], seg.pace$aid_name[ i ], seg.pace$seg.pace.min[ i ], seg.pace$seg.pace.sec[ i ] ) )
seg.pace$seg.names <- c( NA, seg.names )

seg.pace <- filter( seg.pace, aid_distance > 0 )

ggplot( seg.pace, aes( x = aid_distance, y = seg.pace ) ) +
  geom_text( aes( label = seg.names ), nudge_y = 0.4, nudge_x = -seg.pace$seg.dist / 2, size = 3, hjust = 0, alpha = 0.75, check_overlap = F, angle = 90, colour = "#003182" ) +
  geom_vline( xintercept = seg.pace$aid_distance, linetype = "dashed", alpha = 0.25 ) +
  geom_segment( aes(x = aid_distance - seg.dist, xend = aid_distance,
                    y = seg.pace, yend = seg.pace ), size = 2) +
  ylim( c( 10, 38 ) ) +
  xlab( "Distance (miles)" ) +
  ylab( "Empirical 24-Hour Segment Pace (min/mi)" ) +
  ggtitle( "Empirical 24-Hour Splits" )

men.top10 <- wser %>% filter( gender == "M" & gender.final.status == "Top 10" ) %>%
  mutate( ord_name = sprintf( "M%.0f %s", gender.final.pos, display_name ) ) %>%
  arrange( gender.final.pos )

finish.order <- men.top10 %>% select( display_name, ord_name ) %>% distinct()
men.top10$ord_name <- factor( men.top10$ord_name, levels = finish.order$ord_name, ordered = T )

women.top10 <- wser %>% filter( gender == "F" & gender.final.status == "Top 10" ) %>%
  mutate( ord_name = sprintf( "F%.0f %s", gender.final.pos, display_name ) ) %>%
  arrange( gender.final.pos )

finish.order <- women.top10 %>% select( display_name, ord_name ) %>% distinct()
women.top10$ord_name <- factor( women.top10$ord_name, levels = finish.order$ord_name, ordered = T )


ggplot( seg.pace, aes( x = aid_distance, y = seg.pace ) ) +
  geom_text( aes( label = seg.names ), nudge_y = 0.4, nudge_x = -seg.pace$seg.dist / 2, size = 3, hjust = 0, alpha = 0.75, check_overlap = F, angle = 90, colour = "#003182" ) +
  geom_vline( xintercept = seg.pace$aid_distance, linetype = "dashed", alpha = 0.25 ) +
  geom_segment( aes(x = aid_distance - seg.dist, xend = aid_distance,
                    y = seg.pace, yend = seg.pace ), size = 2) +
  geom_segment( data = men.top10, aes(x = aid_distance - seg.dist, xend = aid_distance,
                    y = seg.pace, yend = seg.pace, colour = ord_name ) ) +
  xlab( "Distance (miles)" ) +
  ylab( "Empirical 24-Hour Segment Pace (min/mi)" ) +
  ggtitle( "Empirical 24-Hour Splits (Top 10 Men)" ) +
  scale_color_discrete( name = "Runner Name" ) +
  ylim( c( 7, 40 ) )

ggplot( seg.pace, aes( x = aid_distance, y = seg.pace ) ) +
  geom_text( aes( label = seg.names ), nudge_y = 0.4, nudge_x = -seg.pace$seg.dist / 2, size = 3, hjust = 0, alpha = 0.75, check_overlap = F, angle = 90, colour = "#003182" ) +
  geom_vline( xintercept = seg.pace$aid_distance, linetype = "dashed", alpha = 0.25 ) +
  geom_segment( aes(x = aid_distance - seg.dist, xend = aid_distance,
                    y = seg.pace, yend = seg.pace ), size = 2) +
  geom_segment( data = women.top10, aes(x = aid_distance - seg.dist, xend = aid_distance,
                                      y = seg.pace, yend = seg.pace, colour = ord_name ) ) +
  xlab( "Distance (miles)" ) +
  ylab( "Empirical 24-Hour Segment Pace (min/mi)" ) +
  ggtitle( "Empirical 24-Hour Splits (Top 10 Women)" ) +
  scale_color_discrete( name = "Runner Name" ) +
  ylim( c( 8, 40 ) )

seg.pace %>%
  select( seg.dist, seg.pace.min, seg.pace.sec, seg.names, aid_distance, cutoff.24 )

## Finishing status by aid station

pred3 <- wser %>%
  filter( aid_distance > 0 ) %>%
  mutate( under.24.pace = elapsed.hrs < cutoff.24 ) %>%
  group_by( aid_name, aid_distance, under.24.pace, gender.final.status ) %>%
  summarise( num = n() ) %>%
  mutate( prop = 100 * num / sum( num ) ) %>%
  as.data.frame()

ex <- expand.grid( aid_distance = unique( pred3$aid_distance ),
                   under.24.pace = unique(pred3$under.24.pace),
                   gender.final.status = unique( pred3$gender.final.status),
                   stringsAsFactors = F )

p.data <- ex %>%
  left_join( pred3 ) %>%
  mutate( under.pace = ifelse( under.24.pace, "Under 24-Hr Pace", "Over Under 24-Hr Pace" ) )

p.data$prop[ is.na( p.data$prop ) ] <- 0

ggplot( p.data, aes( x = aid_distance, y = prop, group = gender.final.status, fill = gender.final.status, colour = gender.final.status ) ) +
  geom_area( alpha = 0.60 ) +
  geom_text( aes( label = sprintf( "%.0f%%", prop ) ), position = "stack",
             colour = "black", alpha = 0.75, size = 3, check_overlap = T ) +
  facet_wrap( ~ under.pace ) +
  scale_fill_discrete( name = "Finishing Status" ) +
  scale_colour_discrete( name = "Finishing Status" ) +
  xlab( "Distance (miles)" ) +
  ylab( "Proportion Eventual Finishing Status (%)" ) +
  ggtitle( "Eventual Finishing Status By Pace" )

## Cumulative pace differential

pacing <- wser %>%
  mutate( cum.pace.diff = elapsed.hrs - cutoff.24 ) %>%
  mutate( cum.pace.diff.min = 60 * cum.pace.diff / aid_distance ) %>%
  group_by( bib ) %>%
  mutate( max.diff = max( cum.pace.diff ) ) %>%
  mutate( min.diff = min( cum.pace.diff ) ) %>%
  mutate( was.behind = ifelse( max.diff > 0, "Fell Behind", "Never Behind" ) ) %>%
  mutate( was.ahead = ifelse( min.diff < 0, "Ahead of Pace", "Never Ahead" ) )
  
men <- filter( pacing, gender == "M" )
women <- filter( pacing, gender == "F" )

women %>%
  ungroup() %>%
  filter( gender.final.status == "Top 10" ) %>%
  distinct( bib, was.behind, was.ahead ) %>%
  group_by( was.behind, was.ahead ) %>%
  summarise( num = n() )

ggplot( men, aes( x = aid_distance, y = cum.pace.diff, group = bib, colour = gender.final.status ) ) +
  geom_hline( yintercept = 0, colour = "black", alpha = 0.5, size = 1 ) +
  geom_line() +
  facet_wrap( ~ gender.final.status, scales = "free" ) +
  scale_colour_discrete( name = "Finishing Status" ) +
  xlab( "Distance (miles)" ) +
  ylab( "Cumulative Differential (hours)" ) +
  ggtitle( "Cumulative Time Versus 24-Hour Pace (Men)" )

ggplot( women, aes( x = aid_distance, y = cum.pace.diff, group = bib, colour = gender.final.status ) ) +
  geom_hline( yintercept = 0, colour = "black", alpha = 0.5, size = 1 ) +
  geom_line() +
  facet_wrap( ~ gender.final.status, scales = "free" ) +
  scale_colour_discrete( name = "Finishing Status" ) +
  xlab( "Distance (miles)" ) +
  ylab( "Cumulative Differential (hours)" ) +
  ggtitle( "Cumulative Time Versus 24-Hour Pace (Women)" )

frontier <- pacing %>%
  filter( silver.buckle == T ) %>%
  group_by( aid_name, aid_distance ) %>%
  summarise( cum.pace.diff = max( cum.pace.diff ) + 1/12 )
#  summarise( cum.pace.diff = max( cum.pace.diff ) ) %>%
#  arrange( aid_distance )

men.ahead <- filter( men, min.diff < 0 )
women.ahead <- filter( women, min.diff < 0 )

women.ahead %>% filter( gender.final.status == "DNF" )

men.ahead %>% ungroup() %>% distinct( bib ) %>% summarise( num = n() )

formula <- y ~ poly( x, 5 )
model <- lm( cum.pace.diff ~ poly( aid_distance, 5 ), data = frontier )

ggplot( men.ahead, aes( x = aid_distance, y = cum.pace.diff, group = bib, colour = gender.final.status ) ) +
  geom_hline( yintercept = 0, colour = "black", alpha = 0.5, size = 1 ) +
  geom_smooth( data = frontier, aes( group = NA, colour = NA ), method = "lm",
               se = FALSE, formula = formula, colour = "#999999", size = 2 ) +
  geom_line() +
  facet_wrap( ~ silver.buckle, scales = "free", labeller = buckle_labeller ) +
  scale_colour_discrete( name = "Final Status" ) +
  xlab( "Distance (miles)" ) +
  ylab( "Cumulative Differential (hours)" ) +
  ggtitle( "Cumulative Time Versus 24-Hour Pace (Men)" )

ggplot( women.ahead, aes( x = aid_distance, y = cum.pace.diff, group = bib, colour = gender.final.status ) ) +
  geom_hline( yintercept = 0, colour = "black", alpha = 0.5, size = 1 ) +
  geom_smooth( data = frontier, aes( group = NA, colour = NA ), method = "lm",
               se = FALSE, formula = formula, colour = "#999999", size = 2 ) +
  geom_line() +
  facet_wrap( ~ silver.buckle, scales = "free", labeller = buckle_labeller ) +
  scale_colour_discrete( name = "Final Status" ) +
  xlab( "Distance (miles)" ) +
  ylab( "Cumulative Differential (hours)" ) +
  ggtitle( "Cumulative Time Versus 24-Hour Pace (Women)" )


aid.stations$buffer <- as.vector( predict( model, aid.stations ) )
aid.stations$official.24 <- official.24.pace

aid.stations %>% select( aid_name, aid_distance, cutoff.24, buffer, official.24 )

## Biggest comebacks

wser %>%
  filter( silver.buckle == T, gender == "F" ) %>%
  mutate( cum.pace.diff = elapsed.hrs - cutoff.24 ) %>%
  group_by( display_name, bib ) %>%
  summarise( max.diff = 60 * max( cum.pace.diff ) ) %>%
  arrange( desc( max.diff ) )
  
wser %>%
  mutate( cum.pace.diff = elapsed.hrs - cutoff.24 ) %>%
  filter( display_name == "Jacqueline Merritt" ) %>%
  select( aid_name, aid_distance, cum.pace.diff, silver.prob ) %>%
  arrange( desc( cum.pace.diff ) ) %>%
  head( 1 )

wser %>%
  filter( display_name == "Andrew Stevens" ) %>%
  select( display_name, aid_name, aid_distance, elapsed.hrs, silver.prob )

## Biggest blowup

wser %>%
  filter( silver.buckle == F, gender == "F" ) %>%
  mutate( cum.pace.diff = elapsed.hrs - cutoff.24 ) %>%
  group_by( display_name ) %>%
  summarise( min.diff = min( cum.pace.diff ) ) %>%
  arrange( min.diff )

wser %>%
  mutate( cum.pace.diff = elapsed.hrs - cutoff.24 ) %>%
  filter( display_name == "Sarah Keyes" ) %>%
  select( aid_name, aid_distance, cum.pace.diff, silver.prob ) %>%
  arrange( cum.pace.diff ) %>%
  head( 1 )

wser %>%
  filter( display_name == "Sarah Keyes" & aid_name == "Finish Line" ) %>%
  select( elapsed.hrs )

## Steady Pace

wser %>%
  filter( silver.buckle == T, gender == "F" ) %>%
  mutate( cum.pace.diff = elapsed.hrs - cutoff.24 ) %>%
  group_by( display_name ) %>%
  summarise( max.diff = max( cum.pace.diff ),
             min.diff = min( cum.pace.diff ) ) %>%
  mutate( rg = max.diff + abs( min.diff ) ) %>%
  arrange( rg )

wser %>%
  filter( display_name == "Andrew Stevens" ) %>%
  mutate( pace.diff = round( 60 * ( elapsed.hrs - cutoff.24 ) ) ) %>%
  select( aid_name, aid_distance, pace.diff, silver.prob )

## Foresthill

wser %>% filter( aid_name == "Foresthill" ) %>%
  mutate( pace.diff = round( 60 * ( elapsed.hrs - cutoff.24 ) ) ) %>%
  mutate( ahead.pace = ifelse( pace.diff < 0, T, F ) ) %>%
  group_by( silver.buckle, ahead.pace ) %>%
  summarise( num = n() ) %>%
  mutate( prop = num / sum( num ) )

wser %>%
  filter( silver.buckle == T ) %>%
  filter( aid_name == "Auburn Lake Trails" ) %>%
  arrange( desc( elapsed.hrs ) ) %>%
  select( display_name, elapsed.hrs )

wser %>%
  filter( gender == "F", gender.final.status == "Top 10", aid_name == "Lyon Ridge" ) %>%
  mutate( pace.diff = round( 60 * ( elapsed.hrs - cutoff.24 ) ) ) %>%
  select( display_name, elapsed.hrs, cutoff.24, pace.diff )
  

wser %>%
  filter( gender == "M", silver.buckle == F ) %>%
  mutate( pace.diff = round( 60 * ( elapsed.hrs - cutoff.24 ) ) ) %>%
  filter( aid_name == "Peachstone" ) %>%
  arrange( pace.diff )
  