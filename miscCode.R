
calcSSE <- function(span) {
  loessResults <- loess(tag1$x_pos ~ tag1$seconds, data=tag1, span=span)
  res <- loessResults$residuals
  sse <- sum(res^2)  
  return(sse)
}

optimalSpan <- optim(par =c(0.5), calcSSE, method = "SANN")

calcSSE <- function(y, x, df, span) {
  loessResults <- loess(y ~ x, data=df, span=span)
  res <- loessResults$residuals
  sse <- sum(res^2)  
  return(sse)
}

optimalSpan <- optim(par=c(tag1$x_pos, tag1$seconds, tag1, 0.5), calcSSE, method="SANN")


#specific player datasets
player1 <- df[df$tag_id == 1,]

player1_200_380 <- player1[player1$seconds >200 & player1$seconds < 380,]

ggplot(data = player1,
       aes(x = seconds, y = speed_km.h, color = speed_km.h)) +
  geom_line() +
  scale_colour_gradientn(colours = rainbow(7)) +
  xlab("\n Time (s)") +
  ylab(expression(Velocity ~ (m.s^-1))) +
  scale_x_continuous(limits = c(200, 380), expand = c(0, 0), breaks = seq(200, 380, by = 20)) +
  scale_y_continuous() +
  theme_classic() +
  theme(legend.position = "none") #+ 
facet_wrap(tag_id ~ .)

pitchBG <- function(lengthPitch = 105, widthPitch = 68, fill = "white", colour = "dimgrey", size = 1) 
{
  ggplot() +
    # perimeter line
    geom_rect(aes(xmin = 0, xmax = lengthPitch, ymin = 0, ymax = widthPitch), fill = fill, colour = colour, size = 1) +
    # centre circle
    geom_circle(aes(x0 = lengthPitch/2, y0 = widthPitch/2, r = 9.15), colour = colour, size = 1) +
    # kick off spot
    geom_circle(aes(x0 = lengthPitch/2, y0 = widthPitch/2, r = 0.25), fill = fill, colour = colour, size = 1) +
    # halfway line
    geom_segment(aes(x = lengthPitch/2, y = 0, xend = lengthPitch/2, yend = widthPitch), colour = colour, size = 1) + # penalty arcs
    geom_arc(aes(x0 = 11, y0 = widthPitch/2, r = 9.15, start = 0.65, end = 2.49), colour = colour, size = 1) +
    geom_arc(aes(x0 = lengthPitch - 11, y0 = widthPitch/2, r = 9.15, start = 3.79, end = 5.63), colour = colour, size = 1) +
    # penalty areas
    geom_rect(aes(xmin = 0, xmax = 16.5, ymin = widthPitch/2 - 20.15, ymax = widthPitch/2 + 20.15), fill = NA, colour = colour, size = 1) +
    geom_rect(aes(xmin = lengthPitch - 16.5, xmax = lengthPitch, ymin = widthPitch/2 - 20.15, ymax = widthPitch/2 + 20.15), fill = NA, colour = colour, size = 1) +
    # penalty spots
    geom_circle(aes(x0 = 11, y0 = widthPitch/2, r = 0.25), fill = colour, col = colour, size = 1) +
    geom_circle(aes(x0 = lengthPitch - 11, y0 = widthPitch/2, r = 0.25), fill = colour, colour = colour, size = 1) +
    # six yard boxes
    geom_rect(aes(xmin = 0, xmax = 5.5, ymin = (widthPitch/2) - 9.16, ymax = (widthPitch/2) + 9.16), fill = NA, colour = colour, size = 1) +
    geom_rect(aes(xmin = lengthPitch - 5.5, xmax = lengthPitch, ymin = (widthPitch/2) - 9.16, ymax = (widthPitch/2) + 9.16), fill = NA, colour = colour, size = 1) +
    # goals
    geom_rect(aes(xmin = -2, xmax = 0, ymin = (widthPitch/2) - 3.66, ymax = (widthPitch/2) + 3.66), fill = NA, colour = colour, size = 1) +
    geom_rect(aes(xmin = lengthPitch, xmax = lengthPitch + 2, ymin = (widthPitch/2) - 3.66, ymax = (widthPitch/2) + 3.66), fill = NA, colour = colour, size = 1) +
    coord_fixed() +
    xlab("") +
    ylab("")
}
pitchBG()