library(imager)
dist.color.func <- function(img, col)     ## A function that calculates the distance matrix of an imager image from a given color
{
  col.mat <- as.array(img[, , 1, ])
  dims <- dim(col.mat)
  
  # Calculate distance to given color
  dist <- matrix(0, nrow = dims[1], ncol = dims[2])
  dist <- sqrt((col.mat[ , ,1] - col[1])^2 + (col.mat[ , ,2] - col[2])^2 + (col.mat[ , ,3] - col[3])^2)
  return(dist)
}


dist.white <- numeric(length = length(final_data_grp3$Links))
dist.black <- numeric(length = length(final_data_grp3$Links))
for(i in 1:length(final_data_grp3$Links)){
  print(paste("starting",i))
  dist.white[i] <- mean(dist.color.func(load.image(final_data_grp3$Links[i]),c(1,1,1)))
  dist.black[i] <- mean(dist.color.func(load.image(final_data_grp3$Links[i]),c(0,0,0)))
}

Painter <- rep(c("Oscar_Claude_Monet","Pablo_Ruiz_Picasso","Salvador_Dali","Vincent_Van_Gogh"),
               each = 714)
white_black_df <- data.frame(Painter,dist.white,dist.black)

save(white_black_df, file = "white_black_df.Rdata")
