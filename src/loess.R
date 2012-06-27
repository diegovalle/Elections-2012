ggplot(mpolls, aes(date, value, group = variable, color = variable)) +
  geom_point(size = 2.6) +
  geom_smooth(method = loess) +
  scale_x_date(limits = c(as.Date("2012-03-15"), max(polls$date))) +
  scale_color_manual("candidate", values = c( "red", "#00448b", "#ffd217", 
                                              "lightblue"))
ggsave(file.path("graphs", "loess.svg"), dpi = 100, w = 9, h=6)