library(tidyverse)
library(readxl)

# RNA Qubit Data

df <- read_excel("RNA_qubit_extraction_paper.xlsx")

ggplot() + geom_jitter(data = df, mapping = aes(x = Sample, y = RNA_qubit)) + 
  facet_grid(vars(Method), vars(Sample)) + ylim(0,70) + 
  ylab("Qubit ng/uL") + xlab("") + 
  ggtitle("RNA Qubit Results") + theme(axis.text.x = element_blank())

# Nanodrop Data 260/280

df <- read_excel("Nanodrop_extraction_paper.xlsx")

ggplot() + geom_jitter(data = df, mapping = aes(x = Sample, y = Nanodrop_260_280)) +
  facet_grid(vars(Method), vars(Sample)) + theme(axis.text.x = element_blank()) +
  ggtitle("Nanodrop Results 260/280 - DNA Purity assessment") + ylab("Nanodrop 260/280") + xlab("")

# Nanodrop Data 260/230

df <- read_excel("Nanodrop_extraction_paper.xlsx")

ggplot() + geom_jitter(data = df, mapping = aes(x = Sample, y = Nanodrop_260_230)) +
  facet_grid(vars(Method), vars(Sample)) + theme(axis.text.x = element_blank()) +
  ggtitle("Nanodrop Results 260/230 - Nucleic Acid Purity assessment") + ylab("Nanodrop 260/230") + xlab("")

# qPCR data 

df <- read_excel("qPCR_extraction_paper.xlsx")

ggplot() + geom_jitter(data = df, mapping = aes(x = Primer, y = qPCR_Ct)) + 
  facet_grid(vars(Method),vars(Sample)) + facet_wrap(~Sample) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ggtitle("qPCR Results")

# Frozen/ Not frozen in lysis buffer test 

df <- read_excel("Frozen_notfrozen_test_extract_paper.xlsx")

ggplot() + geom_jitter(data = df, mapping = aes(x = Treatment, y = Result)) +
  facet_grid(vars(Sample), vars(Assay)) + facet_wrap(~Assay) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  xlab("") + ylab("Assay Result") +
  ggtitle("Test to see if samples can be frozen in lysis buffer prior to extraction")


#DNAse Treatment test

df <- read_excel("DNAse_treatment_extract_paper.xlsx")

ggplot() + geom_jitter(data = df, mapping = aes(x = Treatment, y = Result)) +
  facet_grid(vars(Method), vars(Assay)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  xlab("") + ylab("Assay Result") +
  theme(strip.text.x = element_text(size = 6)) +
  ggtitle("Looking at effects of DNA treatments on extraction")

library(tidyverse)
library(readxl)

#Melanogaster all together
df <- read_excel("Melanogaster_extract_paper.xlsx")

ggplot() + geom_jitter(data = df, mapping = aes(x = Method, y = Result)) + facet_wrap(~Assay) + xlab("") +
  ggtitle("Drosophila melanogaster Directzol vs KingFisher Extraction Results")

ggsave("melanogaster_extraction_results.pdf", height=10, width=7, units="in")

#Aedes all together
df <- read_excel("Aedes_exraction_paper.xlsx")

ggplot() + geom_jitter(data = df, mapping = aes(x = Method, y = Result)) + facet_wrap(~Assay) + xlab("") +
  ggtitle("Aedes Aegypti Directzol vs KingFisher Extraction Results")

ggsave("aedes_extraction_results.pdf", height=10, width=7, units="in")

#Cells all together
df <- read_excel("cells_extract_paper.xlsx")

ggplot() + geom_jitter(data = df, mapping = aes(x = Method, y = Result)) + facet_wrap(~Assay) + xlab("") +
  ggtitle("BSRT7/5 Cell Line Directzol vs KingFisher Extraction Results")

ggsave("cells_extraction_results.pdf", height=10, width=7, units="in")

