# adds a column to each divded DF, CITY, that has the city title. Divded by Latitude
austin_data$CITY <- 'Austin'
nash_data$CITY <- 'Nashville'
seat_data$CITY <- 'Seattle'

# creates DF for each city and season, then adds column SEASON for each DF. Redefines city DF's at end with the added SEASON column and a final all-inclusive DF
nash_spring<- (filter(nash_data, DATE_YMD>20130319, DATE_YMD<20130621))
nash_spring$SEASON <- 'Spring'

seat_spring<- (filter(seat_data, DATE_YMD>20130319, DATE_YMD<20130621))
seat_spring$SEASON <- 'Spring'

austin_spring<- (filter(austin_data, DATE_YMD>20130319, DATE_YMD<20130621))
austin_spring$SEASON <- 'Spring'

nash_summer<- (filter(nash_data, DATE_YMD>20130620, DATE_YMD<20130923))
nash_summer$SEASON <- 'Summer'

seat_summer<- (filter(seat_data, DATE_YMD>20130620, DATE_YMD<20130923))
seat_summer$SEASON <- 'Summer'

austin_summer<- (filter(austin_data, DATE_YMD>20130620, DATE_YMD<20130923))
austin_summer$SEASON <- 'Summer'

nash_fall<- (filter(nash_data, DATE_YMD>20130922, DATE_YMD<20131220))
nash_fall$SEASON <- 'Fall'

seat_fall<- (filter(seat_data, DATE_YMD>20130922, DATE_YMD<20131220))
seat_fall$SEASON <- 'Fall'

austin_fall<- (filter(austin_data, DATE_YMD>20130922, DATE_YMD<20131220))
austin_fall$SEASON <- 'Fall'

nash_winter<- (filter(nash_data, DATE_YMD<20130320 | DATE_YMD>20131221))
nash_winter$SEASON <- 'Winter'

seat_winter<- (filter(seat_data, DATE_YMD>20131221 | DATE_YMD<20130320))
seat_winter$SEASON <- 'Winter'

austin_winter<- (filter(austin_data, DATE_YMD>20131221 | DATE_YMD<20130320))
austin_winter$SEASON <- 'Winter'

austin_data<- bind_rows(austin_spring, austin_summer, austin_fall, austin_winter)
nash_data<- bind_rows(nash_spring, nash_summer, nash_fall, nash_winter)
seat_data<- bind_rows(seat_spring, seat_summer, seat_fall, seat_winter)

all_data<- bind_rows(austin_data, nash_data, seat_data)

#Groups by season, finds average rain fall then joins on season 
austin_SEASON <- austin_data %>% group_by(SEASON) %>% summarise(austinPRCP = mean(PRCP))
nash_SEASON <- nash_data %>% group_by(SEASON) %>% summarise(nashPRCP = mean(PRCP))
seat_SEASON <- seat_data %>% group_by(SEASON) %>% summarise(seatPRCP = mean(PRCP))

aust_nash <- inner_join(austin_SEASON, nash_SEASON, by = "SEASON")
SEASON_InnerJoin <- inner_join(aust_nash, seat_SEASON, by = "SEASON")


 
# Finds the max precipitation of each city by date then inner joins all the tables on date  
aust_prcp <- austin_data %>% group_by(DATE_YMD) %>% summarise(austPrcp = max(PRCP))%>% arrange(DATE_YMD)
nash_prcp <- nash_data %>% group_by(DATE_YMD) %>% summarise(nashPrcp = max(PRCP))%>% arrange(DATE_YMD)
seat_prcp <- seat_data %>% group_by(DATE_YMD) %>% summarise(seatPrcp = max(PRCP))%>% arrange(DATE_YMD)


aust_nash <- inner_join(aust_prcp, nash_prcp, by = "DATE_YMD")
prcp_InnerJoin <- inner_join(aust_nash, seat_prcp, by = "DATE_YMD")

#Finds the min and max recorded temperatures for Seattle and Nashville by date. 
nash_TEMP <- nash_data %>% group_by(DATE_YMD) %>% summarise(nashMinTemp = max(TMIN), nashMaxTemp = max(TMAX))%>% arrange(DATE_YMD)
seat_TEMP <- seat_data %>% group_by(DATE_YMD) %>% summarise(seatMinTemp = max(TMIN), seatMaxTemp = max(TMAX))%>% arrange(DATE_YMD)

nash_seatTEMP <- left_join(seat_TEMP, nash_TEMP, by = "DATE_YMD")


