library(readxl)
metpowcalc <- read_excel("metabolic power calculations.xlsx")

speed.df <- metpowcalc[1:2]

speed.df <- speed.df %>%
  mutate(acceleration = c(NA,diff(speed)/diff(time)),
         alpha = rad2deg(atan(9.81/acceleration)),
         g = sqrt(acceleration^2 + 9.81^2),
         equivilantSlope = tan(0.5 * pi - atan(9.81/acceleration)),
         equivilantMass = g/9.81,
         energyCost = ifelse(acceleration == 0, 3.6*1.29, (155.4*equivilantSlope^5 - 30.4*equivilantSlope^4 - 43.3*equivilantSlope^3 + 46.3*equivilantSlope^2 + 19.5*equivilantSlope + 3.6)*equivilantMass*1.29),
         metabolicPower = energyCost*speed)

