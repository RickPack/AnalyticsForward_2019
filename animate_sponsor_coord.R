volun_image  <- image_read("Volunteers.jpg")  # 863x720
plan_image   <- image_read("OtherPlanningCommitteeMembers.jpg") %>% 
                scale_image("863x720")
board_image  <- image_read("BoardMembers.jpg")
lunch_image  <- image_read("NTT_lunch.png") #633x644
p5_image     <- image_read("af_2019-5.png") 
p7_image     <- image_read("af_2019-7.png") 
bc_image     <- image_read("BCBSNC.png")

img <- c(plan_image, lunch_image, p5_image, volun_image, plan_image, board_image,
         p7_image, bc_image)
animate2 <- image_animate(img, fps = 0.25)
image_write(animate2, "AF_sponsor_coord.gif")