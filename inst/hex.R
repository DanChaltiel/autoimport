
library(hexSticker)
library(ggplot2)


d=tibble(
  # x = c(0, 1, -1, 2, -2, 3, -3, 4, -4),
  # y = 1:9,
  x = c(0, 1, -1, 2, -2, 3, -3),
  y = 1:7,
  hjust = case_when(sign(x)==1 ~ 1, sign(x)==-1 ~ 0, .default=0.5),
  # hjust = (sign(x)+1)*0.5-1
)

d = tibble(x = c(0, 1, -1, 2, -2, 3, -3),
           y = 1:7)

p = d %>%
  mutate(x=x*0.1) %>%
  ggplot() +
  aes(x, y, hjust=0.5) +
  geom_text(label="@importFrom", color = "#c26132", family="mono", size=9) +
  scale_x_continuous(expand=c(0.2, 0.2), breaks=scales::breaks_width(1)) +
  scale_y_continuous(expand=c(0.2, 0.2), breaks=scales::breaks_width(1)) +
  theme_void()

sticker(
  #package name
  package="autoimport",
  p_size=20,
  #hexagon
  h_fill = "#323232",
  h_color = "#c26132",
  h_size = 1,
  #subplot
  subplot= p,
  s_x=1, s_y=.75,
  s_width=1.5, s_height=1.1,
  #output
  filename="inst/figures/logo.png"
)




# GIMP background -----------------------------------------------------------------------------

#
# # img = "inst/figures/importfrom.png"
# img = ggplot()+theme_void()
# sticker(
#   img,
#   package="",
#   p_size=16,
#   s_x=1, s_y=.7,
#   h_fill = "#323232",
#   h_color = "#c26132",
#   h_size = 1,
#   s_width=0.8, s_height=1,
#
#   filename="inst/figures/autoimport_bg.png"
# ) %>% print()


