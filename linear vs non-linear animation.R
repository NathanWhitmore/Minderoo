### Linear
library(tidyverse)
library(gganimate)

x <- seq(from = 0, to =10, by = 0.1)

y <- 10 + 0.2 *x

step <- 1:length(x)

df <- data.frame(x,y, step)

p <- ggplot()+
  theme_minimal()+ 
  geom_rect(aes(xmin = 2.5, xmax = 7.5, ymin =10, ymax = Inf), fill = "lightblue", alpha =0.2)+
  geom_point(data = df, aes(x=x, y=y), size = 5, colour = "red")+
  ggtitle("linear")+
  transition_time(step) + 
  ease_aes('cubic-in-out')+
  shadow_wake(wake_length = 0.5, colour = "orange")+
  xlab("Explanatory variable (input)")+
  ylab("Response variable (output)")+
  theme(axis.title = element_text(size = 24, face = 2, colour = "grey"),
        plot.title = element_text(size = 24, face = 2))+
  theme(axis.title.x = element_text(margin = margin(t= 20)))+
  theme(axis.title.x = element_text(margin = margin(r= 20)))+
  theme(axis.text = element_blank())+
  labs(subtitle = " ")


animate(p, nframes = 64, renderer = gifski_renderer("linear.gif"),
        duration = 9)
 


### Non-linear

x <- seq(from = -10, to = 10, by =0.02)

y <- 3000 + 2*x^3 +  0.3 * x^2 + 1*x

ggplot()+
  geom_point(aes(x= x + 100, y=y))+
  ggtitle("non-linear")

step <- 1:length(x)

df <- data.frame(x,y, step)

q <- ggplot()+
  theme_minimal()+
  geom_rect(aes(xmin = -4, xmax = 4, ymin =0, ymax = Inf), fill = "lightblue", alpha =0.2)+
  geom_point(data = df, aes(x=x, y=y), size = 5, colour = "red")+
  ggtitle("non-linear")+
  transition_time(step) + 
  ease_aes('cubic-in-out')+
  shadow_wake(wake_length = 0.5, colour = "orange")+
  xlab("Explanatory variable (input)")+
  ylab("Response variable (output)")+
  theme(axis.title = element_text(size = 24, face = 2, colour = "grey"),
        plot.title = element_text(size = 24, face = 2))+
  theme(axis.title.x = element_text(margin = margin(t= 20)))+
  theme(axis.title.y = element_text(margin = margin(r= 20)))+
  theme(axis.text = element_blank())+
  theme(aspect.ratio = 1)+
  labs(subtitle = " ")
  
animate(q, nframes = 64, renderer = gifski_renderer("linear.gif"),
        duration = 9)
