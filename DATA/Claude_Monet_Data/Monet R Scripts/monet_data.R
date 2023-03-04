library(tidyverse)
library(rvest)
library(imager)
html = read_html("https://commons.wikimedia.org/wiki/Paintings_by_Claude_Monet")

all_links = html %>% html_elements(".floatnone a") %>% html_attr("href")

pic_link = array(0)


for(i in 1:981){
  
  print(paste("starting",i))
  y = read_html(paste("https://commons.wikimedia.org/",all_links[i],sep = ""))
  pic_link[i] = y %>% html_element(".fullImageLink a") %>% html_attr("href")
  
}

names = html %>% html_elements(".image img") %>% html_attr("alt")
names = gsub(".jpg","",names[2:983])

date = html %>% html_table()
date1 = date[[3]]$inception
date2 = as.numeric(date1)


monet_data = data.frame(Names = names, Links = pic_link, Year = date2[1:982])
View(monet_data)

monet_data1 = monet_data %>% filter(!row_number() %in% c(20, 70, 132, 160, 169, 203,
                                                         249, 298, 336, 345, 346, 347,
                                                         348, 396, 411, 423, 428, 462,
                                                         485, 486, 487, 488, 489, 490, 
                                                         501, 518, 528, 593, 663, 666,
                                                         668, 679, 682, 685, 702, 726,
                                                         729, 734, 785, 871, 894, 895,
                                                         896, 910, 918, 947, 951, 969))   ## deleting the rows with image links of larger size


View(monet_data1)

date_monet1 = monet_data1$Year 
date = which(is.na(date_monet1), arr.ind=TRUE)

monet_data2 = monet_data1 %>% filter(!row_number() %in% date)  ## deleting the rows with NA in year column
view(monet_data2)

save(monet_data2, file = "monet_data2.Rdata")


attach(monet_data2)

red = green = blue = r.mean = g.mean = b.mean = r.q1 = r.q2 = r.q3 = b.q1 = b.q2 = b.q3 = g.q1 = g.q2 = g.q3 = QD.red = QD.green = QD.blue = numeric(length = 890)
for(i in 1:890){
  print(paste("starting",i))
  
  col.mat = array(0, dim = dim(load.image(Links[i])[ , ,1, ])) 
  col.mat = as.array((load.image(Links[i])[ , ,1, ]))
 
  red[i] = sd(col.mat[ , ,1])  # red SD
  green[i] = sd(col.mat[ , ,2])  # green SD
  blue[i] = sd(col.mat[ , ,3])  # blue SD
  
  r.mean[i] = mean(col.mat[ , ,1]) # red mean
  g.mean[i] = mean(col.mat[ , ,2]) # green mean
  b.mean[i] = mean(col.mat[ , ,3]) # blue mean

  r.q1[i] = quantile(col.mat[,,1])[2] 
  r.q2[i] = quantile(col.mat[,,1])[3] # Quartiles for red color channel
  r.q3[i] = quantile(col.mat[,,1])[4]
  
  QD.red[i] = (r.q3[i] - r.q1[i])/2
  
  g.q1[i] = quantile(col.mat[,,2])[2]
  g.q2[i] = quantile(col.mat[,,2])[3] # Quartiles for green color channel
  g.q3[i] = quantile(col.mat[,,2])[4]
  
  QD.green[i] = (g.q3[i] - g.q1[i])/2
  
  b.q1[i] = quantile(col.mat[,,3])[2]
  b.q2[i] = quantile(col.mat[,,3])[3] # Quartiles for blue color channel
  b.q3[i] = quantile(col.mat[,,3])[4]
  
  QD.blue[i] = (b.q3[i] - b.q1[i])/2

## Quartile deviations and Medians calculated here were used in getting inferences from plots

}

monet_data3 = data.frame(monet_data2, Red.sd = red, Green.sd = green, Blue.sd = blue,
                         Red.mean = r.mean, Green.mean = g.mean, Blue.mean = b.mean,
                         Red_Q1 = r.q1, Red_Q2 = r.q2, Red_Q3 = r.q3,
                         Green_Q1 = g.q1, Green_Q2 = g.q2, Green_Q3 = g.q3,
                         Blue_Q1 = b.q1, Blue_Q2 = b.q2, Blue_Q3 = b.q3,
                         QD_Red = QD.red, QD_Green = QD.green, QD_Blue = QD.blue)
View(monet_data3)

save(monet_data3, file = "monet_data3.Rdata") 

attach(monet_data3)
dim1 = dim2 = numeric(length = 890)
for(i in 1:890){
  print(paste("starting",i))
  dim1[i] = dim(load.image(Links[i])[,,1,])[1]
  dim2[i] = dim(load.image(Links[i])[,,1,])[2]
}


monet_data4 = data.frame(monet_data3, dim_1 = dim1, dim_2 = dim2)
View(monet_data4)
save(monet_data4, file = "monet_data4.Rdata") 

attach(monet_data4)


monet_data5 = monet_data4 %>% filter(!row_number() %in% c(4,10,16,25,28,30,33,41,43,44,45,48,49,52,53,59,60,62,65,66,72,74,76,77,
                                                          80,81,82,95,105,112,113,116,122,123,124,125,136,141,146,147,156,158,160,
                                                          162,167,168,171,173,174,178,185,189,201,205,207,210,211,223,224,226,234,
                                                          235,242,244,251,256,259,260,265,273,275,278,280,282,293,308,310,311,319,
                                                          323,336,340,341,343,345,346,347,367,374,375,378,379,384,385,395,408,431,
                                                          432,433,450,451,455,458,459,460,461,484,488,502,514,516,517,527,530,532,
                                                          533,535,540,543,552,558,562,577,578,580,586,594,595,596,603,611,612,614,
                                                          619,621,625,626,632,633,641,643,645,653,681,703,708,720,723,729,730,732,
                                                          733,749,752,756,757,763,774,776,780,781,786,787,791,794,797,798,804,818,
                                                          820,824,853,857,864,872,873))

View(monet_data5)
save(monet_data5, file = "monet_data5.Rdata")


pre_final_data_monet <- data.frame(Painting_Names = monet_data5$Names,
                                   Year = monet_data5$Year,
                                   Links = monet_data5$Links,
                                   Red_Median = monet_data5$Red_Q2,
                                   Green_Median = monet_data5$Green_Q2,
                                   Blue_Median = monet_data5$Blue_Q2,
                                   Red_QD = monet_data5$QD_Red,
                                   Green_QD = monet_data5$QD_Green,
                                   Blue_QD = monet_data5$QD_Blue)
View(pre_final_data_monet)                                 

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
  
  ## Calculate distance to given color
  dist <- matrix(0, nrow = dims[1], ncol = dims[2])
  dist <- sqrt((col.mat[ , ,1] - col[1])^2 + (col.mat[ , ,2] - col[2])^2 + (col.mat[ , ,3] - col[3])^2)
  prop <- length(dist[which(dist <= 0.5, arr.ind = TRUE)])/(dims[1]*dims[2])
  return(prop)
}


prop.mat <- matrix(0, nrow = 714, ncol = 16)
for(i in 1:714){
  loaded.img <- load.image(pre_final_data_monet$Links[i])
  print(paste("starting",i))
  for(j in 1:16){
    prop.mat[i,j] = prop.color.new(loaded.img, col.values[j,])
  }
  print(paste("ending",i))
}

## Load the R Workspace named color_names.Rdata

colnames(prop.mat) <- col.names
final_data_monet <- data.frame(pre_final_data_monet, prop.mat)
View(final_data_monet)
save(final_data_monet, file = "final_data_monet.Rdata")


###############################################  the end of monet data  ##########################################################

