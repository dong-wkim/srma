library(ggplot2)
library(readxl)
library(here)

data <- read_excel("G:/My Drive/.github/meta-analysis/data/xlsx/lattice.xlsx")

data$graft_type <- data$group
lattice <- ggplot(data = data, 
                  aes(x = graft_type, 
                      y = estimate.scaled, 
                      shape = graft_type,
                      ymin = ci.lower.scaled, 
                      ymax = ci.upper.scaled)) +
  geom_pointrange(size = 0.2) +
  geom_errorbar(width = 0.02) +
  geom_hline(yintercept = 1, linetype = 2, color = "blue") +
  facet_wrap(~ names, strip.position = "left", ncol=1,nrow = 27, scales = "free_y") +
  xlab("Subgroup") +
  ylab("Pooled Estimate (95% Confidence Interval)") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    strip.text.y.left = element_text(hjust = 0, vjust = 1, angle = 0, face = "bold"))+
    coord_flip()
lattice
print(lattice)

ggsave("lattice.svg", plot = lattice, width = 8, height = 20, dpi = 300)


source("scripts/packages.R")

packages <- c(
  "googledrive",
  "googlesheets4",
  "readxl",
  "openxlsx",
  "tidyverse",
  "dplyr",
  "meta",
  "metafor",
  "ggplot2",
  "ggpubr",
  "ggforestplot",
  "ggplotify",
  "ggplot2",
  "devtools",
  "svglite",
  "here"
)

for (package in packages) {
  if (!requireNamespace(package, quietly = TRUE))
    install.packages(package)
}

devtools::install_github("NightingaleHealth/ggforestplot")
lapply(packages, library, character.only = TRUE)

lattice <- read_excel("data/xlsx/lattice.xlsx")
data <- lattice
# lattice.forest <- function(x) {
#   data <- x
  alim <- c(0,1)
  xlim <- c(-1.25,1.25)
  
  k <- nrow(data)
  outcomes <- k/3
  spacing <- outcomes -1
  header <- 2
  
  ylim <- c(0,k + outcomes + spacing + header)
  ylim[2]
  
  svg(file = "revised_lattice.svg", width = 9, height = 15)
  op <- par(xpd=TRUE)
  
  x <- data$estimate
  forest(
      x = data$estimate.scaled,
      ci.lb = data$ci.lower.scaled,
      ci.ub = data$ci.upper.scaled,
      cex = 0.85,
      top = 2,
      alim = alim, 
      xlim = xlim,
      ylim = c(-3,k+9),
      plim = c(0,1,0.25),
      fonts = "Calibri Light",
      header = "Outcome",
      shade = "zebra",
      xlab = ""
      )
  )
par(op)
dev.off()
  
  # Add additional column header labels
  text(ilab.xpos, k+9, c("N", "Mean", "SD"), pos=2, cex =0.85, font=2)
  
  # Add subgroup title labels
  text(xlim[1], c(k + 7.1, kb + 3.1), pos=4, c("S-QT","B-QT"), cex = 0.85, font =2)
  
  addpoly(res.sqt, 
          row=kb+5.5, 
          mlab=mlabfun("RE Model for Subgroup: ", res.sqt), cex=0.85, addpred=TRUE, efac=efac_value)
  
  addpoly(res.bqt, 
          row= 1.5, 
          mlab=mlabfun("RE Model for Subgroup: ", res.bqt), cex=0.85, addpred=TRUE, efac=efac_value)
  
  text(xlim[1], -2.2, pos=4, cex=0.85, bquote(paste("Test for Subgroup Differences: ",
                                                    italic(Q)[M], " = ", .(fmtx(reg$QM, digits=2)),
                                                    ", df = ", .(reg$p - 1), ", ",
                                                    italic(p),.(fmtp(reg$QMp, digits=3, add0=TRUE, sep=TRUE, equal=TRUE)))))
  
  par(op)
  dev.off()
}

forest.prop <- function(x) {
  # Define file paths
  data_path <- paste0("data/xlsx/dichotomous/",x,".xlsx")
  csv_path <- paste0("data/csv/dichotomous/",x,".csv")
  svg_path <- paste0("plots/forest/dichotomous/",x,".svg")
  
  data <- read_excel(data_path) %>%
    rename(ni = paste0(x, "_n"), xi = paste0(x, "_event")) %>% 
    select(study, graft_type, ni, xi)
  
  data <- escalc(measure = "PLO", ni = ni, xi = xi, data = data, slab = study)
  data <- summary(data,transf=transf.ilogit)
  # data.back <- summary(data, transf=transf.ilogit)
  # data$ilogit.yi <- data.back$yi
  # data$ilogit.ci.lb <- data.back$ci.lb
  # data$ilogit.ci.ub <- data.back$ci.ub
  write.csv(data, file = csv_path, row.names = FALSE)
  
  alim <- c(min(data$ci.lb), max(data$ci.ub))
  alim[1] <- ifelse(alim[1] > 10, floor(alim[1]/5)*5, floor(alim[1]/0.5)*0.5)
  alim[2] <- round(max(data$ci.ub[data$ci.ub != max(data$ci.ub)]), 2)
  #alim[2] <- ifelse(alim[2] > 20, ceiling(alim[2]/5)*5, ceiling(alim[2]/0.5)*0.5)
  xlim <- c(alim[1]-1.65*diff(alim),alim[1]+1.65*diff(alim))
  
  ilab <- function(arg) {
    (3.3*diff(alim)*arg) + xlim[1]
  }
  ilab.xpos <- ilab(c(0.39,0.45))
  
  ks <- sum(data$graft_type == "S-QT")
  kb <- sum(data$graft_type == "B-QT")
  k <- ks + kb
  y <- c(-3,k+9)
  
  m.prop <- metaprop(event = xi, n = ni, studlab = study, data = data, 
                     method = "GLMM", sm = "PLOGIT", 
                     fixed = FALSE, random = TRUE, method.random.ci = "HK", tau.common = FALSE)
  
  res.glmm <- rma.glmm(measure = "PLO", ni = ni, xi = xi, data = data, slab = study)
  pred <- predict(res.glmm, transf = transf.ilogit, digits = 2)
  
  # Generate and save forest plot
  scale_efac <- function(k) {
    if (k < 7) {
      return(c(1.0, 0.8, 0.6))
    } else if (k <= 15) {
      return(c(0.8, 0.6, 0.4))
    } else {
      return(c(0.4, 0.3, 0.2))
    }}
  efac_value <- scale_efac(k)
  
  height = (res.glmm$k + 13)/4
  svg(file = svg_path, width = 9, height = height)
  op <- par(xpd=TRUE)
  
  with(data, {
    forest(yi, ci.lb = ci.lb, ci.ub = ci.ub, slab = study,
           cex = 0.85, top = 2, alim = alim, at = seq(alim[1], alim[2], 0.1),
           xlim = xlim, ylim = c(-3,k+9),
           ilab = cbind(ni, xi), ilab.xpos = ilab.xpos,
           ilab.pos = 2, rows = c(3:(kb + 2), (kb + 7):(k + 6)),
           refline = pred$pred, mlab = "RE Model for All Studies: ",
           plim = c(0.9, 1.2, 0.85), fonts = "Calibri Light",
           header = "Author(s) and Year", 
           efac = efac_value, 
           shade = "zebra",
           xlab = "", digits = 3L
    )
  })
  
  
  # Add additional column header labels
  text(ilab.xpos, k+9, c("N", "Event"), pos = 2, cex = 0.85, font = 2)
  
  # Add subgroup title labels
  text(xlim[1], c(k+7.1, kb+3.1), pos = 4, c("S-QT", "B-QT"), cex = 0.85, font = 2)
  
  # Add summary statistics, polygon, and pooled estimate for subgroup A
  m.prop.sqt <- update(m.prop, subset = graft_type == "S-QT")
  res.sqt <- update(res.glmm, subset = graft_type == "S-QT")
  pred.sqt <- predict(res.sqt, transf = transf.ilogit, digits = 2)
  
  text(xlim[1], kb + 5.5, pos = 4, cex = 0.85, bquote(paste(
    "RE Model for Subgroup: ", italic("\u03c4"^2), " = ", .(fmtx(m.prop.sqt$tau2, digits = 2)), ", ",
    italic(I)^2, " = ", .(fmtx(m.prop.sqt$I2 * 100, digits = 1)), "%, ", 
    italic(p), .(fmtp(m.prop.sqt$pval.Q[1], digits = 3, add0 = TRUE, sep = TRUE, equal = TRUE))
  )))
  addpoly(pred.sqt$pred, ci.lb = pred.sqt$ci.lb, ci.ub = pred.sqt$ci.ub, row = kb + 5.5, efac = 0.3)
  
  
  # Add summary statistics, polygon, and pooled estimate for subgroup B
  m.prop.bqt <- update(m.prop, subset = graft_type == "B-QT")
  res.bqt <- update(res.glmm, subset = graft_type == "B-QT")
  pred.bqt <- predict(res.bqt, transf = transf.ilogit, digits = 2)
  
  text(xlim[1], 1.5, pos = 4, cex = 0.85, bquote(paste(
    "RE Model for Subgroup: ", italic("\u03c4"^2), " = ", .(fmtx(m.prop.bqt$tau2, digits = 2)), ", ",
    italic(I)^2, " = ", .(fmtx(m.prop.bqt$I2 * 100, digits = 1)), "%, ", 
    italic(p), .(fmtp(m.prop.bqt$pval.Q[1], digits = 3, add0 = TRUE, sep = TRUE, equal = TRUE))
  )))
  addpoly(pred.bqt$pred, ci.lb = pred.bqt$ci.lb, ci.ub = pred.bqt$ci.ub, row = 1.5, efac = 0.3)
  
  abline(h = 0)
  
  # Add summary statistics, polygon, and pooled estimate for all studies`
  text(xlim[1], -1, pos = 4, cex = 0.85, bquote(paste(
    "RE Model for All Studies: ", italic("\u03c4"^2), " = ", .(fmtx(m.prop$tau2, digits = 2)), ", ",
    italic(I)^2, " = ", .(fmtx(m.prop$I2 * 100, digits = 1)), "%, ", 
    italic(p), .(fmtp(m.prop$pval.Q[1], digits = 3, add0 = TRUE, sep = TRUE, equal = TRUE))
  )))
  addpoly(pred$pred, ci.lb = pred$ci.lb, ci.ub = pred$ci.ub, row = -1, efac = 0.3)
  
  m.prop.subgroup <- update(m.prop, subgroup = graft_type)
  text(xlim[1], -2.2, pos = 4, cex = 0.85, bquote(paste(
    "Test for Subgroup Differences: ", italic(Q)[M], " = ", .(fmtx(m.prop.subgroup$Q.b.random, digits = 2)),
    ", df = ", .(m.prop.subgroup$df.Q.b.random), ", ", italic(p), .(fmtp(m.prop.subgroup$pval.Q.b.random, digits = 3, add0 = TRUE, sep = TRUE, equal = TRUE))
  )))
  
  par(op)
  dev.off()
}
