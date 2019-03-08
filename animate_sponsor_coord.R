library(magick)
library(dplyr)

#1200x630 found at https://louisem.com/2852/social-media-cheat-sheet-sizes
# in a comment!
res_use <- "1200x630!"

#863x720 original
volun_image  <- image_read("Volunteers.jpg") %>% 
                image_scale(res_use)
plan_image   <- image_read("OtherPlanningCommitteeMembers.jpg") %>% 
                image_scale(res_use)
board_image  <- image_read("BoardMembers.jpg") %>% 
                image_scale(res_use)
#633x644
spons_image  <- image_read("AFBanner2019-2.jpg") %>% 
                image_scale(res_use)
lunch_image  <- image_read("NTT_lunch.png") %>% 
  image_scale(res_use)
p5_image     <- image_read("af_2019-5.png") %>% 
  image_scale(res_use)
p7_image     <- image_read("af_2019-7.png") %>% 
  image_scale(res_use)
r_image      <- image_read("R_code_animate.png") %>% 
  image_scale(res_use)
bc_image     <- image_read("BCBSNC.png") %>% 
  image_scale(res_use)
keynote_image <- image_read("Marketplace_Zillow_JordanMeyer.png") %>% 
  image_scale(res_use)
# Worked when I had two board_image entries after plan_image
# failed with board_image start [plan_image no show]
img <- c(keynote_image, board_image, board_image, plan_image, volun_image,
         spons_image, lunch_image, p5_image, p7_image, r_image, bc_image)
animate2 <- image_animate(img, fps = 0.25)
image_write(animate2, "AF_sponsor_coord.gif")