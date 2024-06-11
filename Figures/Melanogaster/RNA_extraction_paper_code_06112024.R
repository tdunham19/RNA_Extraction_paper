#RNA Extraction Paper Figures
## Made by TD 05.24.24
#######################################################

# load Required Libraries
library(tidyverse)
library(readxl)
library(patchwork)
library(bioanalyzeR)

##################### D.melanogaster Results

# read in excel spreadsheet
df1 <- read_excel("Melanogaster_extract_paper.xlsx")

# subset data
df1qubit<- subset(df1, Assay == "RNA_Qubit")
df1nd80<- subset(df1, Assay == "Nanodrop_260/280")
df1nd30<- subset(df1, Assay == "Nanodrop_260/230")
df1gal<- subset(df1, Assay == "qPCR_Galbut")
df1rpl<- subset(df1, Assay == "qPCR_RpL")

# reorder samples
df1$Method <- factor(df1$Method, levels = c('Bead Protocol', 'Directzol')) 

# make plots 
p_p1 <- ggplot() + geom_jitter(width = 0.1, data = df1qubit, mapping = aes(x = Method, y = Result)) + 
  theme_bw() +
  xlab("") + ggtitle("") + 
  ylab("Concentration (ng/uL)") + 
  facet_wrap(~Group) + 
  scale_y_continuous(limits = c(0, NA))

p_p2 <- ggplot() + geom_jitter(width = 0.1, data = df1gal, mapping = aes(x = Method, y = Result)) + 
  theme_bw() +
  xlab("") + ggtitle("") + 
  ylab("Ct") + 
  facet_wrap(~Group) + 
  scale_y_continuous(limits = c(0, NA))

p_p3 <- ggplot() + geom_jitter(width = 0.1, data = df1rpl, mapping = aes(x = Method, y = Result)) + 
  theme_bw() +
  xlab("") + ggtitle("") + 
  ylab("Ct") + 
  facet_wrap(~Group) + 
  scale_y_continuous(limits = c(0, NA))
    
p_p4 <- ggplot() + geom_jitter(width = 0.1, data = df1nd80, mapping = aes(x = Method, y = Result)) + 
  theme_bw() +
  xlab("") + ggtitle("") + 
  ylab("260/280") + 
  facet_wrap(~Group) + 
  scale_y_continuous(limits = c(0, NA))

p_p5 <- ggplot() + geom_jitter(width = 0.1, data = df1nd30, mapping = aes(x = Method, y = Result)) + 
  theme_bw() +
  xlab("") + ggtitle("") + 
  ylab("260/230") + 
  facet_wrap(~Group) + 
  scale_y_continuous(limits = c(0, NA))




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

df2 <- df %>% mutate (King_Fisher = str_detect(sample.index, "King Fisher"))

dfkingfisher <- subset(df2, name == "Melanogaster King Fisher")
dfdirectzol <- subset(df2, name == "Melanogaster Directzol")

# make plot - king fisher
p_pA <-
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
  scale_fill_manual(values = c("#b3b3b3")) +
  scale_color_manual(values = c("TRUE" = "#b3b3b3", "FALSE" = "#b3b3b3"))

# make plot - directzol
p_pB <- 
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
  scale_fill_manual(values = c("#404040")) +
  scale_color_manual(values = c("TRUE" = "#404040", "FALSE" = "#404040"))

# use patchwork to string plots together
tp_p <- ((p_pA)/(p_pB))

# view big plot
tp_p

# use patchwork to string plots together
big_p <- ((p_p1)/(p_p2)/(p_p3)|(p_p4)/(p_p5)/(tp_p)) + plot_annotation(title = "Drosophila Melanogaster: Bead vs. Directzol RNA Extraction - Comparable", caption = "TD 05.24.2024")

# view big plot
big_p

ggsave("all_melanogaster_ext_06112024.pdf", height=8, width=8, units="in")










#Aedes all together
df <- read_excel("Aedes_exraction_paper.xlsx")

ggplot() + geom_jitter(data = df, mapping = aes(x = Method, y = Result)) + facet_wrap(~Assay) + xlab("") +
  ggtitle("Aedes Aegypti Directzol vs KingFisher Extraction Results")

ggsave("aedes_extraction_results.pdf", height=8, width=8, units="in")

#Cells all together
df <- read_excel("cells_extract_paper.xlsx")

ggplot() + geom_jitter(data = df, mapping = aes(x = Method, y = Result)) + facet_wrap(~Assay) + xlab("") +
  ggtitle("BSRT7/5 Cell Line Directzol vs KingFisher Extraction Results")

ggsave("cells_extraction_results.pdf", height=10, width=7, units="in")

