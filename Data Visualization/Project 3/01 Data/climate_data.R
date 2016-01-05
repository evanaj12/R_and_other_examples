austin_data <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from CLIMATE_DATA_AUSTIN"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521:ORCL', USER='C##cs329e_eaj628', PASS='orcl_eaj628', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
# gets austin climate data from oracle database
nashseat_data <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from CLIMATE_DATA_NASH_SEAT"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521:ORCL', USER='C##cs329e_eaj628', PASS='orcl_eaj628', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
# gets nashville and seattle climate data from database, combined for faster downloads
nash_data<-filter(nashseat_data, LATITUDE<40)
# filters data by latitude into nashville and seattle dataframes
seat_data<-filter(nashseat_data, LATITUDE>40)
