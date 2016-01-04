items <- ggplot(dfitem, aes(x = ARTIST, y = UNIT_PRICE))+geom_point()+facet_wrap(~ARTIST)

