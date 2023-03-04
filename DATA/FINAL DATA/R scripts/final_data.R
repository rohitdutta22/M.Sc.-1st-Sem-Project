## load "final_data_monet.Rdata", "final_data_picasso.Rdata", "final_data_salvador.Rdata", "final_data_vann.Rdata"

temp_final <-rbind(final_data_monet, final_data_picasso, 
                   final_data_salvador, final_data_vann)


artists <- c(rep(c("Oscar_Claude_Monet","Pablo_Ruiz_Picasso",
                   "Salvador_Dali","Vincent_Van_Gogh"), each = 714))




final_data_grp3 <- data.frame("Painter" = artists, temp_final)



library(imager)

dark.col.values <- matrix(c(128,0,0,     # maroon
                            139,0,0,     # dark red
                            165,42,42,   # brown
                            178,34,34,   # firebrick
                            255,69,0,    # orange red
                            128,128,0,   # olive
                            85,107,47,   # dark olive green
                            107,142,35,  # olive drab
                            0,100,0,     # dark green
                            0,128,0,     # green
                            34,139,34,   # forest green
                            46,139,87,   # sea green
                            47,79,79,    # dark slate gray
                            0,128,128,   # teal
                            0,139,139,   # dark cyan
                            25,25,112,   # midnight blue
                            0,0,128,     # navy
                            0,0,139,     # dark blue
                            0,0,205,     # medium blue
                            0,0,255,     # blue
                            138,43,226,  # blue violet
                            75,0,130,    # indigo
                            72,61,139,   # dark slate blue
                            106,90,205,  # slate blue
                            123,104,238, # medium slate blue
                            139,0,139,   # dark magenta
                            148,0,211,   # dark violet
                            153,50,204,  # dark orchid
                            128,0,128,   # purple
                            199,21,133,  # medium violet red
                            255,20,147,  # deep pink
                            139,69,19,   # saddle brown
                            160,82,45,   # sienna
                            112,128,144, # slate gray
                            119,136,153, # light slate gray
                            0,0,0,       # black
                            105,105,105, # dim gray / dim grey
                            128,128,128  # gray / grey
), ncol = 3, byrow = TRUE)/255



medium.col.values <- matrix(c(220,20,60,  # crimson
                            255,0,0,      # red
                            255,99,71,    # tomato
                            255,127,80,   # coral
                            205,92,92,    # Indian red
                            233,150,122,  # dark salmon
                            250,128,114,  # salmon
                            255,140,0,    # dark orange
                            255,165,0,    # orange
                            255,215,0,    # gold
                            184,134,11,   # dark golden rod
                            218,165,32,   # golden rod
                            189,183,107,  # dark khaki
                            154,205,50,   # yellow green
                            124,252,0,    # lawn green
                            127,255,0,    # chartreuse
                            0,255,0,      # lime
                            50,205,50,    # lime green
                            143,188,143,  # dark sea green
                            0,250,154,    # medium spring green
                            0,255,127,    # spring green
                            102,205,170,  # medium aqua marine
                            60,179,113,   # medium sea green
                            32,178,170,   # light sea green
                            0,255,255,    # aqua              
                            0,206,209,    # dark turquoise
                            64,224,208,   # turquoise
                            72,209,204,   # medium turquoise
                            95,158,160,   # cadet blue
                            70,130,180,   # steel blue
                            100,149,237,  # corn flower blue
                            0,191,255,    # deep sky blue
                            30,144,255,   # dodger blue
                            65,105,225,   # royal blue
                            147,112,219,  # medium purple
                            186,85,211,   # medium orchid
                            238,130,238,  # violet
                            255,0,255,    # magenta / fuchsia
                            218,112,214,  # orchid
                            219,112,147,  # pale violet red
                            255,105,180,  # hot pink
                            210,105,30,   # chocolate
                            205,133,63,   # peru
                            244,164,96,   # sandy brown
                            222,184,135,  # burly wood
                            210,180,140,  # tan
                            188,143,143   # rosy brown
), ncol = 3, byrow = TRUE)/255



light.col.values <- matrix(c(240,128,128,  # light coral
                            255,160,122,   # light salmon
                            238,232,170,   # pale golden rod
                            240,230,140,   # khaki
                            255,255,0,     # yellow
                            173,255,47,    # green yellow
                            144,238,144,   # light green
                            152,251,152,   # pale green
                            224,255,255,   # light cyan
                            175,238,238,   # pale turquoise
                            127,255,212,   # aqua marine
                            176,224,230,   # powder blue
                            173,216,230,   # light blue
                            135,206,235,   # sky blue
                            135,206,250,   # light sky blue
                            216,191,216,   # thistle
                            221,160,221,   # plum
                            255,182,193,   # light pink
                            255,192,203,   # pink
                            250,235,215,   # antique white
                            245,245,220,   # beige
                            255,228,196,   # bisque
                            255,235,205,   # blanched almond
                            245,222,179,   # wheat
                            255,248,220,   # corn silk
                            255,250,205,   # lemon chiffon
                            250,250,210,   # light golden rod yellow
                            255,255,224,   # light yellow
                            255,228,181,   # moccasin
                            255,222,173,   # navajo white
                            255,218,185,   # peach puff
                            255,228,225,   # misty rose
                            255,240,245,   # lavender blush
                            250,240,230,   # linen
                            253,245,230,   # old lace
                            255,239,213,   # papaya whip
                            255,245,238,   # sea shell
                            245,255,250,   # mint cream
                            176,196,222,   # light steel blue
                            230,230,250,   # lavender
                            255,250,240,   # floral white
                            240,248,255,   # alice blue
                            248,248,255,   # ghost white
                            240,255,240,   # honeydew
                            255,255,240,   # ivory
                            240,255,255,   # azure
                            255,250,250,   # snow
                            169,169,169,   # dark gray / dark grey
                            192,192,192,   # silver
                            211,211,211,   # light gray / light grey
                            220,220,220,   # gainsboro
                            245,245,245,   # white smoke
                            255,255,255    # white
), ncol = 3, byrow = TRUE)/255 



prop.color.new <- function(img, col)
{
  col.mat <- as.array(img[, , 1, ])
  dims <- dim(col.mat)
  
  # Calculate distance to given color
  dist <- matrix(0, nrow = dims[1], ncol = dims[2])
  dist <- sqrt((col.mat[ , ,1] - col[1])^2 + (col.mat[ , ,2] - col[2])^2 + (col.mat[ , ,3] - col[3])^2)
  prop <- length(dist[which(dist <= 0.3, arr.ind = TRUE)])/(dims[1]*dims[2])
  return(prop)
}


############### Monet

light_shade_prop_monet = numeric(length = 714)
medium_shade_prop_monet = numeric(length = 714)
dark_shade_prop_monet = numeric(length = 714)

light_shade_prop_max_monet = numeric(length = 714)
medium_shade_prop_max_monet = numeric(length = 714)
dark_shade_prop_max_monet = numeric(length = 714)

for(i in 1:714){
  
  print(paste("starting",i))
  loaded.img <- load.image(final_data_grp3$Links[i])
  
  temp_prop_vec1 = numeric(length = nrow(dark.col.values))
  for(j in 1:nrow(dark.col.values)){
    temp_prop_vec1[j] = prop.color.new(loaded.img, dark.col.values[j,])
  }
  dark_shade_prop_monet[i] = mean(temp_prop_vec1)
  
  dark_shade_prop_max_monet[i] = max(temp_prop_vec1)
  
  
  temp_prop_vec2 = numeric(length = nrow(medium.col.values))
  for(j in 1:nrow(medium.col.values)){
    temp_prop_vec2[j] = prop.color.new(loaded.img, medium.col.values[j,])
  }
  medium_shade_prop_monet[i] = mean(temp_prop_vec2)
  
  medium_shade_prop_max_monet[i] = max(temp_prop_vec2)
  
  temp_prop_vec3 = numeric(length = nrow(light.col.values))
  for(j in 1:nrow(light.col.values)){
    temp_prop_vec3[j] = prop.color.new(loaded.img, light.col.values[j,])
  }
  light_shade_prop_monet[i] = mean(temp_prop_vec3)
  
  light_shade_prop_max_monet[i] = max(temp_prop_vec3)
  
  print(paste("ending",i))
}

df_shade_monet <- data.frame(final_data_grp3$Painter[1:714],final_data_grp3$Painting_Names[1:714],
                             final_data_grp3$Year[1:714],final_data_grp3$Links[1:714],
                             dark_shade_prop_monet,dark_shade_prop_max_monet,
                             medium_shade_prop_monet,medium_shade_prop_max_monet,
                             light_shade_prop_monet,light_shade_prop_max_monet)
save(df_shade_monet, file = "Shade_monet.Rdata")




############### Picasso

light_shade_prop_picasso = numeric(length = 714)
medium_shade_prop_picasso = numeric(length = 714)
dark_shade_prop_picasso = numeric(length = 714)

light_shade_prop_max_picasso = numeric(length = 714)
medium_shade_prop_max_picasso = numeric(length = 714)
dark_shade_prop_max_picasso = numeric(length = 714)


for(i in 1:714){
  
  print(paste("starting",i))
  loaded.img <- load.image(final_data_grp3$Links[i+714])
  
  temp_prop_vec1 = numeric(length = nrow(dark.col.values))
  for(j in 1:nrow(dark.col.values)){
    temp_prop_vec1[j] = prop.color.new(loaded.img, dark.col.values[j,])
  }
  dark_shade_prop_picasso[i] = mean(temp_prop_vec1)
  dark_shade_prop_max_picasso[i] = max(temp_prop_vec1)
  
  
  temp_prop_vec2 = numeric(length = nrow(medium.col.values))
  for(j in 1:nrow(medium.col.values)){
    temp_prop_vec2[j] = prop.color.new(loaded.img, medium.col.values[j,])
  }
  medium_shade_prop_picasso[i] = mean(temp_prop_vec2)
  medium_shade_prop_max_picasso[i] = max(temp_prop_vec2)
  
  
  temp_prop_vec3 = numeric(length = nrow(light.col.values))
  for(j in 1:nrow(light.col.values)){
    temp_prop_vec3[j] = prop.color.new(loaded.img, light.col.values[j,])
  }
  light_shade_prop_picasso[i] = mean(temp_prop_vec3)
  light_shade_prop_max_picasso[i] = max(temp_prop_vec3)
  
  
  print(paste("ending",i))
}

df_shade_picasso <- data.frame(final_data_grp3$Painter[715:1428],final_data_grp3$Painting_Names[715:1428],
                               final_data_grp3$Year[715:1428],final_data_grp3$Links[715:1428],
                               dark_shade_prop_picasso,dark_shade_prop_max_picasso,
                               medium_shade_prop_picasso,medium_shade_prop_max_picasso,
                               light_shade_prop_picasso, light_shade_prop_max_picasso)
save(df_shade_picasso, file = "Shade_picasso.Rdata")





################## Salvador Dali

light_shade_prop_salvador = numeric(length = 714)
medium_shade_prop_salvador = numeric(length = 714)
dark_shade_prop_salvador = numeric(length = 714)

light_shade_prop_max_salvador = numeric(length = 714)
medium_shade_prop_max_salvador = numeric(length = 714)
dark_shade_prop_max_salvador = numeric(length = 714)


for(i in 1:714){
  
  print(paste("starting",i))
  loaded.img <- load.image(final_data_grp3$Links[i+1428])
  
  temp_prop_vec1 = numeric(length = nrow(dark.col.values))
  for(j in 1:nrow(dark.col.values)){
    temp_prop_vec1[j] = prop.color.new(loaded.img, dark.col.values[j,])
  }
  dark_shade_prop_salvador[i] = mean(temp_prop_vec1)
  dark_shade_prop_max_salvador[i] = max(temp_prop_vec1)
  
  
  temp_prop_vec2 = numeric(length = nrow(medium.col.values))
  for(j in 1:nrow(medium.col.values)){
    temp_prop_vec2[j] = prop.color.new(loaded.img, medium.col.values[j,])
  }
  medium_shade_prop_salvador[i] = mean(temp_prop_vec2)
  medium_shade_prop_max_salvador[i] = max(temp_prop_vec2)
  
  
  temp_prop_vec3 = numeric(length = nrow(light.col.values))
  for(j in 1:nrow(light.col.values)){
    temp_prop_vec3[j] = prop.color.new(loaded.img, light.col.values[j,])
  }
  light_shade_prop_salvador[i] = mean(temp_prop_vec3)
  light_shade_prop_max_salvador[i] = max(temp_prop_vec3)
  
  
  print(paste("ending",i))
}

df_shade_salvador <- data.frame(final_data_grp3$Painter[1429:2142],final_data_grp3$Painting_Names[1429:2142],
                                final_data_grp3$Year[1429:2142],final_data_grp3$Links[1429:2142],
                                dark_shade_prop_salvador,dark_shade_prop_max_salvador,
                                medium_shade_prop_salvador,medium_shade_prop_max_salvador,
                                light_shade_prop_salvador, light_shade_prop_max_salvador)
save(df_shade_salvador, file = "Shade_salvador.Rdata")




##################### Van Gogh

light_shade_prop_van = numeric(length = 714)
medium_shade_prop_van = numeric(length = 714)
dark_shade_prop_van = numeric(length = 714)

light_shade_prop_max_van = numeric(length = 714)
medium_shade_prop_max_van = numeric(length = 714)
dark_shade_prop_max_van = numeric(length = 714)


for(i in 1:714){
  
  print(paste("starting",i))
  loaded.img <- load.image(final_data_grp3$Links[i+2142])
  
  temp_prop_vec1 = numeric(length = nrow(dark.col.values))
  for(j in 1:nrow(dark.col.values)){
    temp_prop_vec1[j] = prop.color.new(loaded.img, dark.col.values[j,])
  }
  dark_shade_prop_van[i] = mean(temp_prop_vec1)
  dark_shade_prop_max_van[i] = max(temp_prop_vec1)
  
  
  temp_prop_vec2 = numeric(length = nrow(medium.col.values))
  for(j in 1:nrow(medium.col.values)){
    temp_prop_vec2[j] = prop.color.new(loaded.img, medium.col.values[j,])
  }
  medium_shade_prop_van[i] = mean(temp_prop_vec2)
  medium_shade_prop_max_van[i] = max(temp_prop_vec2)
  
  
  temp_prop_vec3 = numeric(length = nrow(light.col.values))
  for(j in 1:nrow(light.col.values)){
    temp_prop_vec3[j] = prop.color.new(loaded.img, light.col.values[j,])
  }
  light_shade_prop_van[i] = mean(temp_prop_vec3)
  light_shade_prop_max_van[i] = max(temp_prop_vec3)
  
  
  print(paste("ending",i))
}

df_shade_van <- data.frame(final_data_grp3$Painter[2143:2856],final_data_grp3$Painting_Names[2143:2856],
                           final_data_grp3$Year[2143:2856],final_data_grp3$Links[2143:2856],
                           dark_shade_prop_van,dark_shade_prop_max_van,
                           medium_shade_prop_van,medium_shade_prop_max_van,
                           light_shade_prop_van, light_shade_prop_max_van)
save(df_shade_van, file = "Shade_van.Rdata")



############################# Combining all the shades data frame corresponding to the painters

df_van <- df_shade_van[,5:10]
View(df_van)
colnames(df_van) = c("dark_shade_prop","dark_shade_prop_max","medium_shade_prop","medium_shade_prop_max","light_shade_prop","light_shade_prop_max")

df_monet <- df_shade_monet[,5:10]
View(df_monet)
colnames(df_monet) = c("dark_shade_prop","dark_shade_prop_max","medium_shade_prop","medium_shade_prop_max","light_shade_prop","light_shade_prop_max")

df_salvador <- df_shade_salvador[,5:10]
View(df_salvador)
colnames(df_salvador) = c("dark_shade_prop","dark_shade_prop_max","medium_shade_prop","medium_shade_prop_max","light_shade_prop","light_shade_prop_max")

df_picasso <- df_shade_picasso[,5:10]
View(df_picasso)
colnames(df_picasso) = c("dark_shade_prop","dark_shade_prop_max","medium_shade_prop","medium_shade_prop_max","light_shade_prop","light_shade_prop_max")

df_combined_shades = rbind(df_monet,df_picasso,df_salvador,df_van)
View(df_combined_shades)


#############################

final_data_grp3 <- data.frame(final_data_grp3,dark_shade_prop = df_combined_shades$dark_shade_prop,
                              medium_shade_prop = df_combined_shades$medium_shade_prop,
                              light_shade_prop = df_combined_shades$light_shade_prop)

save(final_data_grp3, file = "Final_DATA_GRP3.Rdata")            ## Final Data frame with which we worked







