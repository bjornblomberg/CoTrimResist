# FOREST PLOT - sensitivity analysis - at least 75% adherence with treatment

# Load libraries
library(forestplot)
library(readxl)

# Import dataframe
forest <- read_excel("~/Library/CloudStorage/OneDrive-UniversityofBergen/R/Bactrim/r_data/forest_hospexp.xlsx")

# Open graphics device to produce postscript graphics
postscript("figS1.eps", width = 16, height = 8, horizontal = F, onefile = F, paper = "special") #, horizontal = FALSE

# Forest plot
forestplot(forest,
           is.summary = c(F,T,F,T,F,F,F,F,T,T,F, F,T,F,F,F,F,T,T,F, F,T,F,F,F,F,T,T,F),
           xlog = T,
           clip = c(0.3, 5),
           xticks = c(0.06, 0.1, 0.2, 0.4, 1, 2, 6),
           labeltext = c(Time, Cotrimoxazole, Placebo,  RR),
           xticks.digits = 1,
            boxsize = 0.3,
           )|>
  fp_set_style(box = "royalblue",
               line = "darkblue",
               summary = "royalblue") |> 
  fp_add_header(Time = c("", ""),
                Placebo = c("Placebo", "% (n/N)"),
                Cotrimoxazole = c("Cotrimoxazole", "% (n/N)"),
                RR = c("", "RR (95% CI)"))  |>
  fp_decorate_graph(graph.pos = 4) 
grid.text("Cotrimoxazole beneficial", x = unit(0.68, "npc"), y = unit(0.87, "npc"), just = "right", gp = gpar(fontsize=15, fontface = "bold", col = "darkred"))
grid.text("Placebo beneficial", x = unit(0.75, "npc"), y = unit(0.87, "npc"), just = "left", gp = gpar(fontsize=15, fontface = "bold", col = "darkred"))

# Close Graphics device
dev.off()
