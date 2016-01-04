orders <- ggplot(dforder, aes(x = CUSTOMER_ID, y = as.Date(ORDER_DATE, "%Y-%m-%d"), color = factor(ORDER_ID)))+geom_point()+facet_wrap(~CUSTOMER_ID)

