viz3 <- stateGrowth %>% mutate(groPerc=cume_dist(groMax), revPerc=cume_dist(revMax)) %>% ggplot(aes(x=groPerc, y=revPerc, color=STATE)) + geom_point(size=8)
