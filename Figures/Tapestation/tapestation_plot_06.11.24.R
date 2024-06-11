#RNA Extraction Paper - Tapestation Figure
## Made by TD 06.11.24
#######################################################

# load Required Libraries
library(bioanalyzeR)
library(tidyverse)

# read in 
ef <- read.electrophoresis("Extract_paper_HSRNA_XML_file.xml")

# the actual data is in the data element
ef$data

# make plot in ggplot2
df <- ef$data

# 30 seems to be where the lower marker ends
lower_marker_length_max <- 30

# put in sample names
df$sample.index[df$sample.index == "1"] <- "Ladder"
df$sample.index[df$sample.index == "2"] <- "Melanogaster King Fisher"
df$sample.index[df$sample.index == "3"] <- "Melanogaster King Fisher"
df$sample.index[df$sample.index == "4"] <- "Melanogaster King Fisher With DNAse 1"
df$sample.index[df$sample.index == "5"] <- "Melanogaster King Fisher With DNAse 2"
df$sample.index[df$sample.index == "6"] <- "Melanogaster Directzol"
df$sample.index[df$sample.index == "7"] <- "Melanogaster Directzol"
df$sample.index[df$sample.index == "8"] <- "Melanogaster Directzol With DNAse 1"
df$sample.index[df$sample.index == "9"] <- "Melanogaster Directzol With DNAse 2"

# subset data
df$name = df$sample.index

df1 <- df %>% mutate (King_Fisher = str_detect(sample.index, "King Fisher"))

dfkingfisher <- subset(df1, name == "Melanogaster King Fisher")
dfdirectzol <- subset(df1, name == "Melanogaster Directzol")

# make plot - king fisher
ggplot(filter(dfkingfisher, sample.index > 1)) +      # filter sample.index 1, a fake ladder
  geom_line(aes(x=length, y=fluorescence, group=sample.index)) +
  # add in coloring under the lines: 
  # length < lower_marker_mlength_max will produce a T/F value, which we can color with scale_fill_manual
  geom_area(aes(x=length, y=fluorescence, fill = King_Fisher, color = King_Fisher <= lower_marker_length_max)) +
  theme_classic(base_size=10) +
  scale_x_log10() +
  xlab("RNA length (nt)") +
  ylab("Fluorescence (arbitrary units)") +
  theme(legend.position="none") + 
  scale_fill_manual(values = c("#1f77b4")) +
  scale_color_manual(values = c("TRUE" = "#1f77b4", "FALSE" = "#1f77b4"))

ggsave("Melano_KF_tape_06.11.24.pdf", height=10, width=7, units="in")


# make plot - directzol
ggplot(filter(dfdirectzol, sample.index > 1)) +      # filter sample.index 1, a fake ladder
  geom_line(aes(x=length, y=fluorescence, group=sample.index)) +
  # add in coloring under the lines: 
  # length < lower_marker_mlength_max will produce a T/F value, which we can color with scale_fill_manual
  geom_area(aes(x=length, y=fluorescence, fill = King_Fisher, color = King_Fisher <= lower_marker_length_max)) +
  theme_classic(base_size=10) +
  scale_x_log10() +
  xlab("RNA length (nt)") +
  ylab("Fluorescence (arbitrary units)") +
  theme(legend.position="none") + 
  scale_fill_manual(values = c("#aa80ff")) +
  scale_color_manual(values = c("TRUE" = "#aa80ff", "FALSE" = "#aa80ff"))

ggsave("Melano_DZ_tape_06.11.24.pdf", height=10, width=7, units="in")
