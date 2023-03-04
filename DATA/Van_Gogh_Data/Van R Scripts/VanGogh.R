library(rvest)
library(imager)
library(ggplot2)
library(gridExtra)
library(dplyr)

html1 <- read_html("https://www.wikiart.org/en/vincent-van-gogh/all-works/text-list")

pic_name <- html1 %>% html_elements(".painting-list-text-row") %>% html_element("a") %>% html_text()
pic_name

year = html_elements(html1, ".painting-list-text-row span")
year
x = substring(year,8,12)
x
final_year = as.numeric(x[1:1931])
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
vann_data = data.frame(pic_name, final_year, final_links)
View(vann_data)

save(vann_data, file = "vann11.Rdata")


attach(vann_data)

df1 <- vann_data %>% filter(!row_number() %in% c(19,1003,1082,1083,1202,1203,1204,1319,1337)) ## Deleting the rows with image links that couldn't be opened
View(df1)
attach(df1)

red.sd = green.sd = blue.sd = red.mean = green.mean = blue.mean = numeric(length = 1170)
r.q1 = r.q2 = r.q3 = b.q1 = b.q2 = b.q3 = g.q1 = g.q2 = g.q3 = QD.red = QD.green = QD.blue = dim1 = dim2 = numeric(length = 1170)

for(i in 1:length(df1$pic_name)){
  print(paste("starting",i))
  col.mat <- array(0, dim = dim(load.image(df1$final_links[i])[,,1,]))
  col.mat <- as.array((load.image(df1$final_links[i]))[,,1,])
  
  red.sd[i] <- sd(col.mat[,,1])
  green.sd[i] <- sd(col.mat[,,2])  ## Standard Deviation
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

vann22 <- data.frame(df1, red.sd, red.mean,
                     green.sd, green.mean,
                     blue.sd, blue.mean, Red_Q1 = r.q1, Red_Q2 = r.q2, Red_Q3 = r.q3,
                     Green_Q1 = g.q1, Green_Q2 = g.q2, Green_Q3 = g.q3,
                     Blue_Q1 = b.q1, Blue_Q2 = b.q2, Blue_Q3 = b.q3,
                     QD_Red = QD.red, QD_Green = QD.green, QD_Blue = QD.blue,
                     dim_1 = dim1, dim_2 = dim2)

save(vann22, file = "vann22.Rdata")

vec <- c(975:1003,1034:1150,1158:1185,1198:1255,1268:1270,1279:1330,1334,1339,1343,1352:1382,
         1390:1497,1508,1511:1535,1548:1575,1587:1677,1693:1701,1722,1741:1794,1837:1912) ## Most of the pencil sketches were omitted from the dataframe, on an average the paintings with colors were taken
length(vec)


pre_final_data_vann <- data.frame(Painting_Names = vann22$pic_name[vec],
                                  Year = vann22$final_year[vec],
                                  Links = vann22$final_links[vec],
                                  Red_Median = vann22$Red_Q2[vec],
                                  Green_Median = vann22$Green_Q2[vec],
                                  Blue_Median = vann22$Blue_Q2[vec],
                                  Red_QD = vann22$QD_Red[vec],
                                  Green_QD = vann22$QD_Green[vec],
                                  Blue_QD = vann22$QD_Blue[vec])
View(pre_final_data_vann)                              
save(pre_final_data_vann, file = "pre_final_data_vann.Rdata")

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
for(i in 35:714){
  loaded.img <- load.image(pre_final_data_vann$Links[i])
  print(paste("starting",i))
  for(j in 1:16){
    prop.mat[i,j] = prop.color.new(loaded.img, col.values[j,])
  }
  print(paste("ending",i))
}

## Load the R workspace named color_names.Rdata
colnames(prop.mat) <- col.names
final_data_vann <- data.frame(pre_final_data_vann, prop.mat)
View(final_data_vann)        
save(final_data_vann, file = "final_data_vann.Rdata")

#################################### End of Van Data #####################################################
