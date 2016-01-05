dfTOPCOMPANIES <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select COMPANYID, STATE, INDUSTRY, REVENUE, GROWTH from TOPCOMPANIES"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521:ORCL', USER='C##cs329e_nh5797', PASS='orcl_nh5797', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
dfTOPCOMPANIES$STATE <- as.character(dfTOPCOMPANIES$STATE)
dfTOPCOMPANIES$INDUSTRY <- as.character(dfTOPCOMPANIES$INDUSTRY)
dfSTATETOTALS <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select STATE, NUM_FIRMS, NUM_ESTABLISHMENTS, EMPLOYMENT, PAYROLL from STATETOTALS2 where STATE<>\'null\'"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521:ORCL', USER='C##cs329e_nh5797', PASS='orcl_nh5797', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
dfSTATETOTALS$STATE <- as.character(dfSTATETOTALS$STATE)
dfSTATETOTALS$STATE[ dfSTATETOTALS$NUM_FIRMS == 172118] <- "MI"
#fixes mislabled state MI
dfSTATEHOUSING <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from STATEHOUSING where STATE<>\'DC\'"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521:ORCL', USER='C##cs329e_nh5797', PASS='orcl_nh5797', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
dfSTATEHOUSING$STATE <- as.character(dfSTATEHOUSING$STATE)
