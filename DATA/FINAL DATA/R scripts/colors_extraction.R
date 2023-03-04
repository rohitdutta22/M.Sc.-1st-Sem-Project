library(rvest)
html <- read_html("https://www.rapidtables.com/web/color/RGB_Color.html")
col.table <- html %>% html_table()
col.names <- col.table[[4]][[2]]
col.names[7] = "Cyan"
col.names[8] = "Magenta"

save(col.names, file = "color_names.Rdata")

col.hex <- col.table[[4]][[3]]
save(col.hex, file = "col_hex.Rdata")


col.values <- col.table[[4]][[4]]
library(stringr)

col.values <- matrix(c(0,0,0,
                       255,255,255,
                       255,0,0,
                       0,255,0,
                       0,0,255,
                       255,255,0,
                       0,255,255,
                       255,0,255,
                       192,192,192,
                       128,128,128,
                       128,0,0,
                       128,128,0,
                       0,128,0,
                       128,0,128,
                       0,128,128,
                       0,0,128), nrow = 16, ncol = 3, byrow = TRUE)/255
