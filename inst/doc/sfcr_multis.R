## ----setup--------------------------------------------------------------------
library(sfcr)
library(tidyverse)

## -----------------------------------------------------------------------------
eqs <- sfcr_set(
  TXs ~ TXd,
  YD ~ W * Ns - TXs,
  Cd ~ alpha1 * YD + alpha2 * Hh[-1],
  Hh ~ YD - Cd + Hh[-1],
  Ns ~ Nd,
  Nd ~ Y / W,
  Cs ~ Cd,
  Gs ~ Gd,
  Y ~ Cs + Gs,
  TXd ~ theta * W * Ns,
  Hs ~ Gd - TXd + Hs[-1]
)

external <- sfcr_set(
  Gd ~ 20, 
  W ~ 1, 
  alpha1 ~ 0.6, 
  alpha2 ~ 0.4, 
  theta ~ 0.2
  )

## -----------------------------------------------------------------------------
baseline <- sfcr_baseline(eqs, external, periods = 60, hidden = c("Hh" = "Hs"))

## -----------------------------------------------------------------------------
x_external <- sfcr_expand(external, alpha1, seq(0.5, 0.9, 0.02))

## -----------------------------------------------------------------------------
mlt_sim1 <- sfcr_multis(x_external, eqs, 50)

## -----------------------------------------------------------------------------
str(mlt_sim1[[1]])

## -----------------------------------------------------------------------------
shock1 <- sfcr_shock(
  variables = sfcr_set(
    alpha2 ~ 0.3
  ),
  start = 5,
  end = 50
)

x_shock <- sfcr_expand(shock1, alpha2, seq(0.1, 0.4, 0.05))

mlt_sim2 <- sfcr_multis(x_shock, baseline, 50)

## -----------------------------------------------------------------------------
str(mlt_sim2[[1]])

## -----------------------------------------------------------------------------
mlt_sim3 <- sfcr_multis(mlt_sim1, fixed = shock1, periods = 50)

## ---- fig.height=9, fig.width=7-----------------------------------------------
mlt_sim1 %>%
  bind_rows() %>%
  pivot_longer(cols = -c(period, simulation)) %>%
  filter(name %in% c("Y", "YD", "Cd")) %>%
  ggplot(aes(x = period, y = value)) +
  geom_line(aes(color = as_factor(simulation))) +
  facet_wrap(~name, nrow = 3) +
  scale_color_hue("alpha1",
                  labels = as.character(seq(0.5, 0.9, 0.02))) +
  theme(legend.position = "bottom")


