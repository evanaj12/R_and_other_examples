artist <- ggplot(dfjoined, aes(x=as.Date(ORDER_DATE, "%Y-%m-%d"), y=as.Date(SHIPPED_DATE, "%Y-%m-%d"), color=ARTIST)) + geom_point() + facet_wrap(~CUSTOMER_STATE)
