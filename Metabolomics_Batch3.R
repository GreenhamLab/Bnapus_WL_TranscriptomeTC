# Reworking a slightly updated version of the Bnapus metabolomics dataset with a couple GSLs added

require(readxl)
require(tidyverse)

setwd("~/Desktop/Git/Bnapus_WL_TC/Revisions")


## Bring in and clean the data
dat = read_xlsx("20241216_Batch3_ExtendedRTdata.xlsx", sheet = "Reorganize")

# Get rid of the .mzML extension
dat$IDs = gsub(".mzML", "", dat$IDs)
# And the Peak area string since it's no longer needed
dat$IDs = gsub(" Peak.area", "", dat$IDs)


# Split the sample names into unique identifiers
  # First transpose to put the samples into rows
gsl = dat

# Separate them out into informative columns
gsl = gsl %>% separate(IDs, c("Genotype", "Treatment", "BioRep", "Position", "TechRep"), 
                 extra = "merge", remove = FALSE)


# Pivot to put all the GSLs into one column for plotting
  # Pull out the Unknowns and the Sucrose column for now
NonGSL = gsl[, c(1:6, 9,12,14, 19:23)]
gsl = gsl[, -c(9,12,14,19)]

#gsl = gsl %>% pivot_longer(cols = c(8:18), names_to = "GSL", values_to = "PeakArea")

## Write this out and manually add in ZT from the master sheet I have
  # Also manually doing the technical normalization in excel 
write.csv(gsl, "20241216_PartwayCleaned_Batch3_GSLs.csv")


##### FIRST CHECKPOINT ##### ##### ##### ##### ##### 
## Bring it back in and double check things
CleanedGSL = read_xlsx("20241216_PartwayCleaned_Batch3_GSLs.xlsx", sheet = "TechNorm")

levels(factor(CleanedGSL$TechRep))
levels(factor(CleanedGSL$BioRep))
levels(factor(CleanedGSL$ZT))
levels(factor(CleanedGSL$Genotype))

CleanedGSL = CleanedGSL %>% pivot_longer(cols = c(8:9), names_to = "GSL", values_to = "PeakArea")


# Log transform the technically normalized data
  ##### BE SURE TO PUT THE APPROPRIATE BASE; R DOES WEIRD THINGS WITH LOGS #####
CleanedGSL$LogArea = log2(CleanedGSL$PeakArea)
CleanedGSL$LogArea = gsub("-Inf", NA, CleanedGSL$LogArea)
CleanedGSL$LogArea = as.numeric(CleanedGSL$LogArea)

## Add a 'Class' column to differentiate between the GSL classes for plotting
#levels(factor(CleanedGSL$GSL))

#CleanedGSL = CleanedGSL %>% mutate(Class = case_when(GSL == "4ME or NEO lg" ~ "Indolic", 
 #                                      GSL == "4ME or NEO sm" ~ "Indolic", 
  #                                     GSL == "4OH" ~ "Indolic",
   #                                    GSL == "GBC" ~ "Indolic",
    #                                   GSL == "GBN" ~ "Indolic", 
     #                                  GSL == "Glucobrassicanapin" ~ "Aliphatic", 
      #                                 GSL == "IBE" ~ "Aliphatic", 
       #                                GSL == "NAP" ~ "Aliphatic",
        #                               GSL == "PRO" ~ "Aliphatic",
         #                              GSL == "SIN" ~ "Aliphatic",
          #                             GSL == "NAS" ~ "Aromatic", 
           #                            TRUE ~ NA))

## Calculate the total GSL content by class
#ClassGSL = CleanedGSL %>% group_by(Genotype, Treatment, ZT, Class, BioRep) %>%
 # mutate(BioRepSum = sum(LogArea, na.rm = TRUE)) %>% ungroup() %>%
 # group_by(Genotype, Treatment, ZT, GSL, BioRep) %>%
 # mutate(GSLSum = sum(LogArea, na.rm = TRUE)) %>% ungroup()

## Plot the peak area totals
#ClassGSL %>% 
 # filter(Genotype == "St") %>% 
  #ggplot(aes(x = factor(ZT), y = BioRepSum, color = Treatment, fill = Treatment)) + 
  #geom_boxplot(alpha = 0.4, position = position_dodge(width = 0.75)) + 
  #geom_point(
  #  aes(group = Treatment), 
  #  position = position_dodge(width = 0.75), # Align points with boxplots
  #  size = 2, 
  #  alpha = 0.8
#  ) + 
 # scale_color_manual(values = c("chartreuse4", "darkgoldenrod2")) +
  #scale_fill_manual(values = c("chartreuse4", "darkgoldenrod2")) +
  #theme_bw() +
  #facet_grid(~Class) +
  #ggtitle("St all GSLs (TechRep3 only, data normalized)")


## Color by GSL to see if the points group by GSL or if there's some other explanation for the variation/subpops
#CleanedGSL %>% 
#  filter(Genotype == "St") %>% 
#  ggplot(aes(x = factor(ZT), y = LogArea, color = GSL, fill = GSL)) + 
#  geom_boxplot(alpha = 0.4, position = position_dodge(width = 0.75)) + 
#  theme_bw() +
#  facet_grid(~Treatment) +
#  ggtitle("St all GSLs (TechRep3; consensus data)") 


## Print them on individual pages by genotpe
GSL_List = split(CleanedGSL, CleanedGSL$GSL)

pdf("Batch3_RTextended_TechNorm_GSLs.pdf",width = 10,height = 5)
for(i in 1:length(GSL_List)) {
  print(GSL_List[[i]] %>% 
          filter(Genotype == "St") %>%
          ggplot(aes(x = factor(ZT), y = LogArea, color = factor(Treatment), fill = factor(Treatment))) +
          geom_boxplot(alpha = .6) +
          geom_point(alpha = .8, position = position_dodge(.75)) +
          scale_color_manual(values = c("chartreuse4", "darkgoldenrod2")) +
          scale_fill_manual(values = c("chartreuse4", "darkgoldenrod2")) +
          theme_classic() +
          xlab("ZT (hours after lights on)") +
          ylab("Log2 Peak Area") +
    ggtitle(paste("St:", names(GSL_List)[i])) +
    theme(legend.position = "bottom"))
  
  print(GSL_List[[i]] %>% 
          filter(Genotype == "Mu") %>%
          ggplot(aes(x = factor(ZT), y = LogArea, color = factor(Treatment), fill = factor(Treatment))) +
          geom_boxplot(alpha = .6) +
          geom_point(alpha = .8, position = position_dodge(.75)) +
          scale_color_manual(values = c("chartreuse4", "darkgoldenrod2")) +
          scale_fill_manual(values = c("chartreuse4", "darkgoldenrod2")) +
          theme_classic() +
          xlab("ZT (hours after lights on)") +
          ylab("Log2 Peak Area") +
    ggtitle(paste("Mu:", names(GSL_List)[i])) +
    theme(legend.position = "bottom"))
}
dev.off()




##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### Removing outliers

setwd("~/Desktop/Git/Bnapus_WL_TC/Revisions/Consensus_RerunsReplaced_OnlyConsensusAcrossTechRepsKept/")
# Older copy of this excel in the 'from mac mini' folder
CleanedGSL = read_xlsx("20241213_TechNorm_GSLs.xlsx", sheet = "Exclude1_TechRep1")

levels(factor(CleanedGSL$TechRep))
levels(factor(CleanedGSL$BioRep))
levels(factor(CleanedGSL$GSL))
levels(factor(CleanedGSL$ZT))
levels(factor(CleanedGSL$Genotype))

# Log transform the technically normalized data
##### BE SURE TO PUT THE APPROPRIATE BASE; R DOES WEIRD THINGS WITH LOGS #####
CleanedGSL$LogArea = log2(CleanedGSL$TechNorm_ColorByTreatment)
CleanedGSL$LogArea = gsub("-Inf", NA, CleanedGSL$LogArea)
CleanedGSL$LogArea = as.numeric(CleanedGSL$LogArea)

## Add a 'Class' column to differentiate between the GSL classes for plotting
levels(factor(CleanedGSL$GSL))

CleanedGSL = CleanedGSL %>% mutate(Class = case_when(GSL == "4ME or NEO sm" ~ "Indolic", 
                                                     GSL == "4OH" ~ "Indolic",
                                                     GSL == "GBC" ~ "Indolic",
                                                     GSL == "GBN" ~ "Indolic", 
                                                     GSL == "Glucobrassicanapin" ~ "Aliphatic", 
                                                     GSL == "IBE" ~ "Aliphatic", 
                                                     GSL == "NAP" ~ "Aliphatic",
                                                     GSL == "PRO" ~ "Aliphatic",
                                                     GSL == "SIN" ~ "Aliphatic",
                                                     GSL == "NAS" ~ "Aromatic", 
                                                     TRUE ~ NA))
levels(factor(CleanedGSL$Class))

# Plot with labels to inspect potential outliers
require(ggrepel)

## Remove the two sample flagged from conditional formatting 
CleanedGSL = CleanedGSL %>% filter()
gsl_List = split(CleanedGSL, CleanedGSL$GSL)

pdf("TechRep1_Mu_OutlierDetect.pdf")
for(i in 1:length(gsl_List)){
  print(gsl_List[[i]] %>% 
          filter(Genotype == "Mu") %>%
          #filter(LogArea_GSL != "-Inf") %>%
          ggplot(aes(x = factor(ZT, levels = c("4", "12", "20")), 
                     y = LogArea, color = Treatment, fill = Treatment)) +
          geom_boxplot(alpha = .6, outlier.colour = NA, outlier.fill = NA) +
          geom_point(aes(group = Treatment), position=position_jitterdodge(dodge.width = 1)) +
          geom_text_repel(aes(label = IDs), max.overlaps = 30) +
          scale_color_manual(values = c("chartreuse4", "darkgoldenrod2")) +
          scale_fill_manual(values = c("chartreuse4", "darkgoldenrod2")) +
          theme_bw() +
          xlab("") +
          ylab("Normalized Peak Area (log)") +
          theme(legend.position = "bottom") +
          ggtitle(paste("Mu:", names(gsl_List[i]))))
}
dev.off()


##### ##### ##### ##### ##### ##### ##### ##### 
##### MAKE THE FINAL PLOTS
CleanedGSL = read_xlsx("20241213_TechNorm_GSLs.xlsx", sheet = "Final_TechRep1")

levels(factor(CleanedGSL$TechRep))
levels(factor(CleanedGSL$BioRep))
levels(factor(CleanedGSL$GSL))
levels(factor(CleanedGSL$ZT))
levels(factor(CleanedGSL$Genotype))

# Log transform the technically normalized data
##### BE SURE TO PUT THE APPROPRIATE BASE; R DOES WEIRD THINGS WITH LOGS #####
CleanedGSL$LogArea = log2(CleanedGSL$TechNorm_ColorByTreatment)
CleanedGSL$LogArea = gsub("-Inf", NA, CleanedGSL$LogArea)
CleanedGSL$LogArea = as.numeric(CleanedGSL$LogArea)

## Add a 'Class' column to differentiate between the GSL classes for plotting
levels(factor(CleanedGSL$GSL))

CleanedGSL = CleanedGSL %>% mutate(Class = case_when(GSL == "4ME or NEO sm" ~ "Indolic", 
                                                     GSL == "4OH" ~ "Indolic",
                                                     GSL == "GBC" ~ "Indolic",
                                                     GSL == "GBN" ~ "Indolic", 
                                                     GSL == "Glucobrassicanapin" ~ "Aliphatic", 
                                                     GSL == "IBE" ~ "Aliphatic", 
                                                     GSL == "NAP" ~ "Aliphatic",
                                                     GSL == "PRO" ~ "Aliphatic",
                                                     GSL == "SIN" ~ "Aliphatic",
                                                     GSL == "NAS" ~ "Aromatic", 
                                                     TRUE ~ NA))
levels(factor(CleanedGSL$Class))

# Plot with labels to inspect potential outliers
require(ggrepel)

## Make a list to loop through and plot
gsl_List = split(CleanedGSL, CleanedGSL$GSL)


pdf("FinalTechRep1_Mu.pdf")
for(i in 1:length(gsl_List)){
  print(gsl_List[[i]] %>% 
          filter(Genotype == "Mu") %>%
          ggplot(aes(x = factor(ZT), y = LogArea, color = factor(Treatment), fill = factor(Treatment))) +
          geom_boxplot(alpha = .6) +
          geom_point(alpha = .8, position = position_dodge(.75)) +
          scale_color_manual(values = c("chartreuse4", "darkgoldenrod2")) +
          scale_fill_manual(values = c("chartreuse4", "darkgoldenrod2")) +
          theme_classic() +
          xlab("ZT (hours after lights on)") +
          ylab("Log2 Peak Area") +
          ggtitle(paste("Mu:", names(gsl_List)[i]), "Tech Rep1") +
          theme(legend.position = "bottom"))
}
dev.off()


## Just checking one GSL with the old way of logging the data 
test = CleanedGSL
test = test %>% mutate(OldLog = log(PeakArea))

test_List = split(test, test$GSL)
pdf("TestingOldLog_FinalTechRep1_Mu.pdf")
for(i in 1:length(test_List)){
  print(gsl_List[[i]] %>% 
          filter(Genotype == "Mu") %>%
          ggplot(aes(x = factor(ZT), y = LogArea, color = factor(Treatment), fill = factor(Treatment))) +
          geom_boxplot(alpha = .6) +
          geom_point(alpha = .8, position = position_dodge(.75)) +
          scale_color_manual(values = c("chartreuse4", "darkgoldenrod2")) +
          scale_fill_manual(values = c("chartreuse4", "darkgoldenrod2")) +
          theme_classic() +
          xlab("ZT (hours after lights on)") +
          ylab("Log? Peak Area") +
          ggtitle(paste("Mu:", names(test_List)[i]), "Tech Rep1") +
          theme(legend.position = "bottom"))
}
dev.off()
