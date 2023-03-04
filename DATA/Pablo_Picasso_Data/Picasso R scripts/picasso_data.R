library(rvest)
library(imager)
library(ggplot2)
library(gridExtra)
library(dplyr)

html1 <- read_html("https://www.wikiart.org/en/pablo-picasso/all-works/text-list")



first_links <- paste("https://www.wikiart.org",html1 %>% html_elements(".painting-list-text-row") %>% html_element("a") %>% html_attr("href"),sep = "")

final_links <- array(0)


## loop starts
for(i in 1:1093){
  print(paste("starting",i))
  
  html2 <- read_html(first_links[i])
  
  final_links[i] <- html2 %>% html_element(".wiki-layout-artist-image-wrapper.btn-overlay-wrapper-artwork") %>% html_element("img") %>% html_attr("src")
  
  
}



pic_name <- html1 %>% html_elements(".painting-list-text-row") %>% html_element("a") %>% html_text()

year <- html1 %>% html_elements(".painting-list-text-row") %>% html_element("span") %>% html_text()

year <- as.numeric(gsub(", ","",year))

picasso_data <- data.frame(painting_Name = pic_name[1:1093],Year = year[1:1093],Links =final_links)

View(picasso_data)

picasso_data_modified1 <- picasso_data %>% filter(!row_number() %in% c(63, 64, 67, 152, 153, 154, 194, 199,
                                                                       292, 334, 382, 523, 558, 693, 825, 826, 827, 828, 829, 902, 903, 909, 1002))     ## Deleting the rows with image links that couldn't be opened

save(picasso_data_modified1, file = "Picasso_modified1.Rdata")
View(picasso_data_modified1)

##############################################################################

attach(picasso_data_modified1)


red.sd = green.sd = blue.sd = red.mean = green.mean = blue.mean = numeric(length = 1070)
for(i in 1:1070){                                       ## Calculating standard deviations and means of each rgb matrices paintings  (But ultimately we didn't use them in the final data)
  print(paste("starting",i))
  col.mat <- as.array((load.image(Links[i]))[,,1,])
  red.sd[i] <- sd(col.mat[,,1])
  green.sd[i] <- sd(col.mat[,,2])
  blue.sd[i] <- sd(col.mat[,,3])
  red.mean[i] <- mean(col.mat[,,1])
  green.mean[i] <- mean(col.mat[,,2])
  blue.mean[i] <- mean(col.mat[,,3])
}

picasso_data.modified2 <- data.frame(picasso_data_modified1, red.sd, red.mean,
                                     green.sd, green.mean,
                                     blue.sd, blue.mean)
View(picasso_data.modified2)
save(picasso_data.modified2, file = "Picasso_modified2.Rdata")
attach(picasso_data.modified2)





r.q1 = r.q2 = r.q3 = b.q1 = b.q2 = b.q3 = g.q1 = g.q2 = g.q3 = QD.red = QD.green = QD.blue = dim1 = dim2 = numeric(length = 1070)
for(i in 1:1070){                                         ##Calculating all the quartiles, quartile deviations and dimensions of each rgb matrices paintings  (we used medians and quartile deviations in the final data frame)
  print(paste("starting",i))
  
  col.mat <- array(0, dim = dim(load.image(Links[i])[,,1,])) 
  col.mat <- as.array((load.image(Links[i])[,,1,]))
  
  r.q1[i] <- quantile(col.mat[,,1])[2]
  r.q2[i] <- quantile(col.mat[,,1])[3]
  r.q3[i] <- quantile(col.mat[,,1])[4]
  
  QD.red[i] <- (r.q3[i] - r.q1[i])/2
  
  g.q1[i] <- quantile(col.mat[,,2])[2]
  g.q2[i] <- quantile(col.mat[,,2])[3]
  g.q3[i] <- quantile(col.mat[,,2])[4]
  
  QD.green[i] = (g.q3[i] - g.q1[i])/2
  
  b.q1[i] <- quantile(col.mat[,,3])[2]
  b.q2[i] <- quantile(col.mat[,,3])[3]
  b.q3[i] <- quantile(col.mat[,,3])[4]
  
  QD.blue[i] = (b.q3[i] - b.q1[i])/2
  
  dim1[i] <- dim(load.image(Links[i])[,,1,])[1]
  dim2[i] <- dim(load.image(Links[i])[,,1,])[2]
}

picasso_data.modified3 <- data.frame(picasso_data.modified2, Red_Q1 = r.q1, Red_Q2 = r.q2, Red_Q3 = r.q3,
                                     Green_Q1 = g.q1, Green_Q2 = g.q2, Green_Q3 = g.q3,
                                     Blue_Q1 = b.q1, Blue_Q2 = b.q2, Blue_Q3 = b.q3,
                                     QD_Red = QD.red, QD_Green = QD.green, QD_Blue = QD.blue,
                                     dim_1 = dim1, dim_2 = dim2)
View(picasso_data.modified3)
save(picasso_data.modified3, file = "Picasso_modified3.Rdata")






pre_final_data_picasso <- data.frame(Painting_Names = picasso_data.modified3$painting_Name[1:714],
                                     Year = picasso_data.modified3$Year[1:714],
                                     Links = picasso_data.modified3$Links[1:714],
                                     Red_Median = picasso_data.modified3$Red_Q2[1:714],
                                     Green_Median = picasso_data.modified3$Green_Q2[1:714],
                                     Blue_Median = picasso_data.modified3$Blue_Q2[1:714],
                                     Red_QD = picasso_data.modified3$QD_Red[1:714],
                                     Green_QD = picasso_data.modified3$QD_Green[1:714],
                                     Blue_QD = picasso_data.modified3$QD_Blue[1:714])
View(pre_final_data_picasso)                                 


col.values <- matrix(c(0,0,0,          ##Black
                       255,255,255,    ##White
                       255,0,0,        ##Red
                       0,255,0,        ##Lime
                       0,0,255,        ##Blue
                       255,255,0,      ##Yellow
                       0,255,255,      ##Cyan
                       255,0,255,      ##Magenta
                       192,192,192,    ##Silver
                       128,128,128,    ##Gray
                       128,0,0,        ##Maroon
                       128,128,0,      ##Olive
                       0,128,0,        ##Green
                       128,0,128,      ##Purple
                       0,128,128,      ##Teal
                       0,0,128),       ##Navy
                     nrow = 16, ncol = 3, byrow = TRUE)/255


prop.color.new <- function(img, col)
{
  col.mat <- as.array(img[, , 1, ])
  dims <- dim(col.mat)
  
  # Calculate distance to given color
  dist <- matrix(0, nrow = dims[1], ncol = dims[2])
  dist <- sqrt((col.mat[ , ,1] - col[1])^2 + (col.mat[ , ,2] - col[2])^2 + (col.mat[ , ,3] - col[3])^2)
  prop <- length(dist[which(dist <= 0.5, arr.ind = TRUE)])/(dims[1]*dims[2])
  return(prop)
}


prop.mat <- matrix(0, nrow = 714, ncol = 16)
for(i in 1:714){
  loaded.img <- load.image(pre_final_data_picasso$Links[i])
  print(paste("starting",i))
  for(j in 1:16){
    prop.mat[i,j] = prop.color.new(loaded.img, col.values[j,])
  }
  print(paste("ending",i))
}

## Load the R workspace named color_names.Rdata
colnames(prop.mat) <- col.names                          
final_data_picasso <- data.frame(pre_final_data_picasso, prop.mat)                
View(final_data_picasso)
save(final_data_picasso, file = "final_data_picasso.Rdata")



##################################################### End of Picasso Data ################################################################
