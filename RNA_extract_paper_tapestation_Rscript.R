# Installing Bioanalyze R package
install.packages(c("XML", "base64enc", "plyr", "png", "ggplot2"))
install.packages("https://github.com/jwfoley/bioanalyzeR/releases/download/v0.10.0/bioanalyzeR_0.10.0-no_data.tar.gz", repos = NULL)

library(bioanalyzeR)
library(tidyverse)

ef <- read.electrophoresis("Extract_paper_HSRNA_XML_file.xml")

# the actual data is in the data element
ef$data

# make some plots with the plots built-in to bioanalyzeR
?qplot.electrophoresis
qplot.electrophoresis(ef)  
qplot.electrophoresis(ef,  log="x", scales="free_y", y = "fluorescence", x="relative.distance")
qplot.electrophoresis(ef,  log="x", y = "fluorescence", show.peaks = "markers", normalize = "total")
qplot.electrophoresis(ef,  log="x", scales="free_y", y = "fluorescence", show.peaks = "markers", normalize = "total")

# make our own plots in ggplot2
df <- ef$data

# 30 seems to be where the lower marker ends
lower_marker_length_max <- 30

ggplot(filter(df, sample.index > 1)) +        # filter sample.index 1, a fake ladder
  geom_line(aes(x=length, y=fluorescence, group=sample.index)) +
  # add in coloring under the lines: 
  # length < lower_marker_mlength_max will produce a T/F value, which we can color with scale_fill_manual
  geom_area(aes(x=length, y=fluorescence, fill = length <= lower_marker_length_max)) +
  scale_fill_manual(values=c("grey50", "lightsteelblue")) +
  # get rid of the ugly legend
  theme_classic(base_size=10) +
  # scale_x_log10(lim = c(0,1000)) +
  scale_x_log10() +
  xlab("RNA length (nt)") +
  ylab("Fluorescence (arbitrary units)") +
  facet_wrap(~sample.index, ncol=1) +
  theme(legend.position="none",
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank()) 

ggsave("tapestation_plot.pdf", height=10, width=7, units="in")


#Adding Sample Names
library(bioanalyzeR)
library(tidyverse)

ef <- read.electrophoresis("Extract_paper_HSRNA_XML_file.xml")

# the actual data is in the data element
ef$data

# make some plots with the plots built-in to bioanalyzeR
?qplot.electrophoresis
qplot.electrophoresis(ef)  
qplot.electrophoresis(ef,  log="x", scales="free_y", y = "fluorescence", x="relative.distance")
qplot.electrophoresis(ef,  log="x", y = "fluorescence", show.peaks = "markers", normalize = "total")
qplot.electrophoresis(ef,  log="x", scales="free_y", y = "fluorescence", show.peaks = "markers", normalize = "total")

# make our own plots in ggplot2
df1 <- ef$data

# 30 seems to be where the lower marker ends
lower_marker_length_max <- 30

df1$sample.index[df1$sample.index == "1"] <- "Ladder"

df1$sample.index[df1$sample.index == "2"] <- "Melanogaster King Fisher No DNAse Sample 1"

df1$sample.index[df1$sample.index == "3"] <- "Melanogaster King Fisher No DNAse Sample 2"

df1$sample.index[df1$sample.index == "4"] <- "Melanogaster King Fisher With DNAse Sample 1"

df1$sample.index[df1$sample.index == "5"] <- "Melanogaster King Fisher With DNAse Sample 2"

df1$sample.index[df1$sample.index == "6"] <- "Melanogaster Directzol No DNAse Sample 1"

df1$sample.index[df1$sample.index == "7"] <- "Melanogaster Directzol No DNAse Sample 2"

df1$sample.index[df1$sample.index == "8"] <- "Melanogaster Directzol With DNAse Sample 1"

df1$sample.index[df1$sample.index == "9"] <- "Melanogaster Directzol With DNAse Sample 2"

ggplot(filter(df1, sample.index > 1)) +        # filter sample.index 1, a fake ladder
  geom_line(aes(x=length, y=fluorescence, group=sample.index)) +
  # add in coloring under the lines: 
  # length < lower_marker_mlength_max will produce a T/F value, which we can color with scale_fill_manual
  geom_area(aes(x=length, y=fluorescence, fill = length <= lower_marker_length_max)) +
  scale_fill_manual(values=c("grey50", "lightsteelblue")) +
  # get rid of the ugly legend
  theme_classic(base_size=10) +
  # scale_x_log10(lim = c(0,1000)) +
  scale_x_log10() +
  xlab("RNA length (nt)") +
  ylab("Fluorescence (arbitrary units)") +
  facet_wrap(~sample.index, ncol=1) +
  theme(legend.position="none",
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank()) 

ggsave("tapestation_plot with sample names.pdf", height=10, width=7, units="in")

#Not facet wrapped

library(bioanalyzeR)
library(tidyverse)

ef <- read.electrophoresis("Extract_paper_HSRNA_XML_file.xml")

# the actual data is in the data element
ef$data

# make some plots with the plots built-in to bioanalyzeR
?qplot.electrophoresis
qplot.electrophoresis(ef)  
qplot.electrophoresis(ef,  log="x", scales="free_y", y = "fluorescence", x="relative.distance")
qplot.electrophoresis(ef,  log="x", y = "fluorescence", show.peaks = "markers", normalize = "total")
qplot.electrophoresis(ef,  log="x", scales="free_y", y = "fluorescence", show.peaks = "markers", normalize = "total")

# make our own plots in ggplot2
df1 <- ef$data

# 30 seems to be where the lower marker ends
lower_marker_length_max <- 30

df1$sample.index[df1$sample.index == "1"] <- "Ladder"

df1$sample.index[df1$sample.index == "2"] <- "Melanogaster King Fisher No DNAse Sample 1"

df1$sample.index[df1$sample.index == "3"] <- "Melanogaster King Fisher No DNAse Sample 2"

df1$sample.index[df1$sample.index == "4"] <- "Melanogaster King Fisher With DNAse Sample 1"

df1$sample.index[df1$sample.index == "5"] <- "Melanogaster King Fisher With DNAse Sample 2"

df1$sample.index[df1$sample.index == "6"] <- "Melanogaster Directzol No DNAse Sample 1"

df1$sample.index[df1$sample.index == "7"] <- "Melanogaster Directzol No DNAse Sample 2"

df1$sample.index[df1$sample.index == "8"] <- "Melanogaster Directzol With DNAse Sample 1"

df1$sample.index[df1$sample.index == "9"] <- "Melanogaster Directzol With DNAse Sample 2"

df2 <- df1 %>% mutate (King_Fisher = str_detect(sample.index, "King Fisher"))

ggplot(filter(df2, sample.index > 1)) +        # filter sample.index 1, a fake ladder
  geom_line(aes(x=length, y=fluorescence, group=sample.index)) +
  # add in coloring under the lines: 
  # length < lower_marker_mlength_max will produce a T/F value, which we can color with scale_fill_manual
  geom_area(aes(x=length, y=fluorescence, fill = King_Fisher, color = King_Fisher <= lower_marker_length_max)) +
  # get rid of the ugly legend
  theme_classic(base_size=10) +
  # scale_x_log10(lim = c(0,1000)) +
  scale_x_log10() +
  xlab("RNA length (nt)") +
  ylab("Fluorescence (arbitrary units)") +
  scale_color_manual(values=c("#999999", "#56B4E9")) +
  theme(legend.position="right",
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank()) + 
  scale_fill_discrete(labels=c('Directzol', 'King Fisher'))

ggsave("tapestation_plot_not_facet_wrapped.pdf", height=10, width=7, units="in")
