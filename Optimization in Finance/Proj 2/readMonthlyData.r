filename = 'N100Monthly.csv'

##############Run readData.R first #########################

# reads in the data
mdata = read.csv(filename,header = TRUE,stringsAsFactors=FALSE)

# clean up data
mdate = apply(as.matrix(mdata$date),MARGIN = 1,FUN = 'toString')
mdate = as.Date(mdate,'%Y%m%d')

mticker = mdata$TICKER
mprice = mdata$PRC
mshares = mdata$SHROUT
mticker[mticker == 'FOXA'] = 'NWSA' 


unique_mdates = sort(unique((mdate)))
unique_mtickers = sort(unique(mticker))

idx = is.element(unique_mtickers,unique_tickers)

if(!all(idx)){
  print('Warning: Some tickers seem to be missing')
}

MonthlyPriceMat = matrix(NA,length(unique_mdates),length(unique_tickers))

for(i in 1:length(unique_tickers)){
  tic = unique_tickers[i]
  idx = is.element(unique_mdates,mdate[mticker == tic])
  MonthlyPriceMat[idx,i] = mprice[mticker == tic]
}
