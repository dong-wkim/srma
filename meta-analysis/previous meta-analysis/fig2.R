


getwd()
data_path <- paste0("data/xlsx/")

# custom function to import datasets in either csv or xlsx format
dread <- function(file_name,file_format) {
  x <- paste0(deparse(substitute(file_name)))
  xlsx_path <- paste0("data/xlsx/",x,".xlsx")
  csv_path <- paste0("data/csv/",x,".csv")
  
  if (file_format == "xlsx") {
    data <- read_excel(xlsx_path)
  } else if (file_format == "csv") {
    data <- read.csv(csv_path)
  }
}

# use custom function to import entire dataset
data <- dread(lattice_data,"xlsx")
View(data)  

# # group by subsets 
# prom <- data[data$subset == "prom",]
# muscle <- data[data$subset == "muscle",]
# stability <- data[data$subset == "stability",]
# complications <- data[data$subset == "complications",]

# store as variables in global environment according to figure number
a <- as.data.frame(data[data$esska == "a",])
b <- as.data.frame(data[data$esska == "b",])
b
c <- as.data.frame(data[data$esska == "c",])
View(a)

# 2. DEFINE VARIABLES --------- 8==============D~~~~~~~~~~~

# rowrs

a.rows <- a$rows
b.rows <- b$rows
c.rows <- c$rows
b.rows
b.slab
c.slab
# a.rows <- c(40, 39, 38, 35, 34, 33, 30, 29, 28, 25, 24, 23, 19, 18, 17, 15, 14, 13, 11, 10, 9, 7, 6, 5, 3, 2, 1)
# b.rows <- c(30, 29, 28, 26, 25, 24, 22, 21, 20, 16, 15, 14, 12, 11, 10, 8, 7, 6, 3, 2, 1)
# c.rows <- c(53, 52, 51, 48, 47, 46, 42, 41, 40, 38, 37, 36, 32, 31, 30, 28, 27, 26, 23, 22, 21, 18, 17, 16, 13, 12, 11, 8, 7, 6, 3, 2, 1)

# alim 

alim <- c(min(data$ci.lower.scaled), max(data$ci.upper.scaled))
alim[1] <- ifelse(alim[1] > 10, floor(alim[1]/5)*5, floor(alim[1]/0.5)*0.5)
alim[2] <- ifelse(alim[2] > 20, ceiling(alim[2]/5)*5, ceiling(alim[2]/0.5)*0.5)
alim # default <- c(0.4,1.0)


# xlim 
xlim <- c(alim[1]-1.65*diff(alim),alim[1]+1.65*diff(alim)) # idk prob have to change
xlim # default <- c(-0.31,1.36)

# ylim

# outcomes <- k/3
# spacing <- outcomes -1
# header <- 2

ylim <- function(x) {
  ylim <- c(0,max(a$rows) + 3)
}

a.ylim <- ylim(a)
b.ylim <- ylim(b)
c.ylim <- ylim(c)

# ilab
ilab <- function(arg) {
  (3.3*diff(xlim)*arg) + alim[1]
}

ilab.xpos <- ilab(c(0.39,0.45)) # positioning of the two annotation columns at 39% and 45% of xlim value based on alim

# paste0(deparse(substitute(df)))

# assign <- function(x,y){
#   var <- paste0(deparse(substitute(x)),".",deparse(substitute(y)))
#   assign(var,x,envir = .GlobalEnv)
# }
# 
# assign(a,height)
# 
# height <- function(x) {
#   var <- assign(x,height)
#   var <- (nrow(x) + 13)/4
#   return(var)
#   }

a.height <- (nrow(a) + 13)/4
b.height <- (nrow(b) + 13)/4
c.height <- (nrow(c) + 13)/4

a.slab <- a$slab
a.slab
a.slab <- c("  S-QT", "  B-QT", "  QT", "  S-QT", "  B-QT", "  QT", "  S-QT", "  B-QT", "  QT", "  S-QT", "  B-QT", "  QT", "    S-QT", "    B-QT", "    QT", "    S-QT", "    B-QT", "    QT", "    S-QT", "    B-QT", "    QT", "    S-QT", "    B-QT", "    QT", "    S-QT", "    B-QT", "    QT")
b.slab <- c("    S-QT", "    B-QT", "    QT", "    S-QT", "    B-QT", "    QT", "    S-QT", "    B-QT", "    QT", "    S-QT", "    B-QT", "    QT", "    S-QT", "    B-QT", "    QT", "    S-QT", "    B-QT", "    QT", "  S-QT", "  B-QT", "  QT", "  S-QT", "  B-QT", "  QT", "  S-QT", "  B-QT", "  QT", "    S-QT", "    B-QT", "    QT", "    S-QT", "    B-QT", "    QT", "    S-QT", "    B-QT", "    QT", "    S-QT", "    B-QT", "    QT")
c.slab <- c("  S-QT", "  B-QT", "  QT", "  S-QT", "  B-QT", "  QT", "  S-QT", "  B-QT", "  QT", "  S-QT", "  B-QT", "  QT", "  S-QT", "  B-QT", "  QT")

svg(file = "figure_2.svg", width = 9, height = 15)
op <- par(xpd=TRUE)

scale_efac <- function(k) {
  if (k < 7) {
    return(c(1.0, 0.8, 0.6))
  } else if (k <= 15) {
    return(c(0.8, 0.6, 0.4))
  } else {
    return(c(0.4, 0.3, 0.2))
  }}
k <- nrow(b)
efac_value <- scale_efac(k)

b.slab <- c("    S-QT", "    B-QT", "    QT", "    S-QT", "    B-QT", "    QT", "    S-QT", "    B-QT", "    QT", "    S-QT", "    B-QT", "    QT", "    S-QT", "    B-QT", "    QT", "    S-QT", "    B-QT", "    QT", "  S-QT", "  B-QT", "  QT", "  S-QT", "  B-QT", "  QT", "  S-QT", "  B-QT", "  QT", "    S-QT", "    B-QT", "    QT", "    S-QT", "    B-QT", "    QT", "    S-QT", "    B-QT", "    QT", "    S-QT", "    B-QT", "    QT")

data <- b

# Set default font before plot function
windowsFonts(CalibriLight = windowsFont("Calibri Light"))
# par(family = "CalibriLight")
library(showtext)
font_add("CalibriLight", "C:/Windows/Fonts/calibril.ttf")
showtext_auto()

#par(family = "CalibriLight")

# plot
lattice <- with(b,
                forest(
                  x = estimate.scaled,
                  ci.lb = ci.lower.scaled,
                  ci.ub = ci.upper.scaled,
                  slab = b.slab,
                  order = order,
                  cex = 0.9,
                  top = 2,
                  # alim = alim, 
                  # xlim = xlim,
                  ylim = b.ylim,
                  plim = c(0,1,0.25),
                  rows = b.rows,
                  family = "Arial",
                  header = "Outcome",
                  efac = efac_value,
                  xlab = "",
                  ilab = cbind(b$k,b$n), 
                  # ilab.xpos = ilab.xpos,
                  ilab.lab = c("k","N"),
                  psize = 0.8,
                  ilab.pos = 2)
)

lattice
xlim <- lattice$xlim
alim <- lattice$alim


# header labels
# text(x,y,pos,text vec,cex,font)
# header label y-axis text positions

a.y <- c(41, 36, 31, 26, 21, 20, 16, 12, 8, 4)
b.y <- c(54, 49, 44, 43, 39, 34, 33, 29, 32, 31, 27, 23, 18, 17, 13, 9, 4)
c.y <- c(24, 19, 14, 9, 4)

# header label text
a.header <- c("IKDC Subjective", "Lysholm", "Tegner", "Marx", "KOOS", "  ADL", "  Pain", "  Sport", "  Symptoms", "  QoL")
b.header <- c("Q-LSI", "  Isometric", "  Isokinetic 60°/s", "  Isokinetic 180°/s", "H-LSI", "  Isometric", "  Isokinetic 60°/s", "  Isokinetic 180°/s", "SLTH","Instrumental Laxity", "IKDC Objective", "Lachman", "  Grade ≥ 1+", "  Grade ≥ 2+", "Pivot Shift", "  Grade ≥ 1+", "  Grade ≥ 2+")
c.header <- c("VAS", "Graft rupture", "Donor site morbidity", "Arthrofibrosis", "Patellar fracture")

for (y in b.y) {
  rect(
    xleft   = xlim[1],
    ybottom = y - 0.5,
    xright  = xlim[2],
    ytop    = y + 0.5,
    col     = "gray90",
    border  = NA
  )
}

text(xlim[1], 
     b.y, 
     pos = 4,b.header, cex = 0.9, font = 2, family="Arial")

par(op)
dev.off()
