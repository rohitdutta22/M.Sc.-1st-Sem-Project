
library(rvest)
library(imager)
library(ggplot2)
library(gridExtra)
library(dplyr)

html1 <- read_html("https://www.wikiart.org/en/salvador-dali/all-works/text-list")
pic_name <- html1 %>% html_elements(".painting-list-text-row") %>% html_element("a") %>% html_text()
pic_name

year = html_elements(html1, ".painting-list-text-row span")
year
x = substring(year,8,12)
x
final_year = as.numeric(x[1:1178])
final_year

first_links <- paste("https://www.wikiart.org",html1 %>% html_elements(".painting-list-text-row") %>% html_element("a") %>% html_attr("href"),sep = "")
first_links
final_links <- array(0)

## loop starts
for(i in 1:length(first_links)){
  print(paste("starting",i))
  
  html2 <- read_html(first_links[i])
  
  final_links[i] <- html2 %>% html_element(".wiki-layout-artist-image-wrapper.btn-overlay-wrapper-artwork") %>% html_element("img") %>% html_attr("src")
}
salvador_data = data.frame(pic_name, final_year, final_links)
View(salvador_data)
attach(salvador_data)



df1 <- salvador_data %>% filter(!row_number() %in% c(78,118,203,300,337,689,759,817))
View(df1)
length(df1)



attach(df1)
red.sd = green.sd = blue.sd = red.mean = green.mean = blue.mean = numeric(length = 1170)
r.q1 = r.q2 = r.q3 = b.q1 = b.q2 = b.q3 = g.q1 = g.q2 = g.q3 = QD.red = QD.green = QD.blue = dim1 = dim2 = numeric(length = 1170)

for(i in 1:1170){
  print(paste("starting",i))
  col.mat <- array(0, dim = dim(load.image(df1$final_links[i])[,,1,]))
  col.mat <- as.array((load.image(df1$final_links[i]))[,,1,])
  
  red.sd[i] <- sd(col.mat[,,1])
  green.sd[i] <- sd(col.mat[,,2]) ## Standard Deviation
  blue.sd[i] <- sd(col.mat[,,3])
  
  red.mean[i] <- mean(col.mat[,,1])
  green.mean[i] <- mean(col.mat[,,2]) ## Mean
  blue.mean[i] <- mean(col.mat[,,3])
  
  r.q1[i] <- quantile(col.mat[,,1])[2]
  r.q2[i] <- quantile(col.mat[,,1])[3] ## Quartiles for red color channel
  r.q3[i] <- quantile(col.mat[,,1])[4]
  
  QD.red[i] <- (r.q3[i] - r.q1[i])/2
  
  g.q1[i] <- quantile(col.mat[,,2])[2]
  g.q2[i] <- quantile(col.mat[,,2])[3] ## Quartiles for green color channel
  g.q3[i] <- quantile(col.mat[,,2])[4]
  
  QD.green[i] = (g.q3[i] - g.q1[i])/2
  
  b.q1[i] <- quantile(col.mat[,,3])[2]
  b.q2[i] <- quantile(col.mat[,,3])[3] ## Quartiles for blue color channel
  b.q3[i] <- quantile(col.mat[,,3])[4]
  
  QD.blue[i] = (b.q3[i] - b.q1[i])/2
  
  dim1[i] <- dim(load.image(df1$final_links[i])[,,1,])[1]
  dim2[i] <- dim(load.image(df1$final_links[i])[,,1,])[2]
}


salvador2 <- data.frame(df1, red.sd, red.mean,
                        green.sd, green.mean,
                        blue.sd, blue.mean, Red_Q1 = r.q1, Red_Q2 = r.q2, Red_Q3 = r.q3,
                        Green_Q1 = g.q1, Green_Q2 = g.q2, Green_Q3 = g.q3,
                        Blue_Q1 = b.q1, Blue_Q2 = b.q2, Blue_Q3 = b.q3,
                        QD_Red = QD.red, QD_Green = QD.green, QD_Blue = QD.blue,
                        dim_1 = dim1, dim_2 = dim2)
View(salvador2)

salvador2 <- salvador2 %>% filter(!row_number() %in% c(456)) # Removing the row corresponding which the image link was not opening
View(salvador2)

save(salvador2, file = "salvador2.Rdata")

pre_final_data_salvador <- data.frame(Painting_Names = salvador2$pic_name[1:714],
                                      Year = salvador2$final_year[1:714],
                                      Links = salvador2$final_links[1:714],
                                      Red_Median = salvador2$Red_Q2[1:714],
                                      Green_Median = salvador2$Green_Q2[1:714],
                                      Blue_Median = salvador2$Blue_Q2[1:714],
                                      Red_QD = salvador2$QD_Red[1:714],
                                      Green_QD = salvador2$QD_Green[1:714],
                                      Blue_QD = salvador2$QD_Blue[1:714])
View(pre_final_data_salvador)                              
save(pre_final_data_salvador, file = "pre_final_data_salvador.Rdata")

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

## load col.names

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
  loaded.img <- load.image(pre_final_data_salvador$Links[i])
  print(paste("starting",i))
  for(j in 1:16){
    prop.mat[i,j] = prop.color.new(loaded.img, col.values[j,])
  }
  print(paste("ending",i))
}

## Load the R workspace named color_names.Rdata
colnames(prop.mat) <- col.names
final_data_salvador <- data.frame(pre_final_data_salvador, prop.mat)
View(final_data_salvador)        
save(final_data_salvador, file = "final_data_salvador.Rdata")

######################################## End of Salvador Data #######################################
