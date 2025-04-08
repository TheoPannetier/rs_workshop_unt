library(RangeShiftR)
library(terra)
library(RColorBrewer)
library(viridis)
library(grid)
library(gridExtra)
library(readr) # For reading .txt files
library(ggplot2)
library(tidyverse)

# relative path from working directory:
dirpath = "Tutorial_02_NeutralGenetics/"

dir.create(paste0(dirpath), showWarnings = TRUE)
dir.create(paste0(dirpath,"Inputs"), showWarnings = TRUE)
dir.create(paste0(dirpath,"Outputs"), showWarnings = TRUE)
dir.create(paste0(dirpath,"Output_Maps"), showWarnings = TRUE)

# Make sure to import the required Inputs from Tutorial 02 !!!



################


landsc <- terra::rast(paste0(dirpath, "Inputs/landscape_10m_batch.txt"))

# Plot land cover map and highlight cells with initial species distribution - option 2 with categorical legend:
landsc.f <- as.factor(landsc)
# add the land cover classes to the raster attribute table
rat <- levels(landsc.f)[[1]][-2]
rat[["landcover"]] <- c("semi-natural broad-leaved woodland", "planted/felled broad-leaved and mixed woodland", "heathland, marshy grassland", "unimproved grassland", "planted/felled coniferous woodland", "improved grasslands, arable, water", "roads, buildings")
levels(landsc.f) <- rat

plot(landsc.f, axes = F, col=brewer.pal(n = 7, name = "Spectral"))





############################

patch <- terra::rast(paste0(dirpath, "Inputs/woodland_1ha_patchIDs.txt"))

# We can have a glimpse at how many cells the different patches contain:
table(values(patch))

#############################

# Plot the patches in different colours:
plot(patch, axes=F, legend=F,
     col = c('black',rep(brewer.pal(n = 12, name = "Paired"),5))
) 

###########################

patch30 <- terra::rast(paste0(dirpath, "Inputs/patch30.txt"))

# Look at initial patch:
plot(patch30, type="continuous")

##################

land <- ImportedLandscape(LandscapeFile = "landscape_10m_batch.txt",
                          PatchFile = "woodland_1ha_patchIDs.txt", 
                          Resolution = 10,
                          Nhabitats = 7,
                          K_or_DensDep = c(10, rep(0,6)),
                          SpDistFile = "patch30.txt",
                          SpDistResolution = 10)


#####################


(trans_mat <- matrix(c(0, 1, 0, 0, 0.1, 0.4, 5, 0, 0.8), nrow = 3, byrow = F))



############

stg <- StageStructure(Stages=3,           # 1 juvenile + 2 adult stages
                      TransMatrix=trans_mat, 
                      MaxAge=1000, 
                      SurvSched=2, 
                      FecDensDep=T)
demo <- Demography(StageStruct = stg,
                   ReproductionType = 1)  # simple sexual model




disp <-  Dispersal(
  Emigration = Emigration(
    DensDep = T,
    StageDep = T,
    EmigProb = cbind(0:2, c(0.5, 0, 0), c(10.0, 0, 0), c(1.0, 0, 0))
  ), 
  Transfer = SMS(
    PR = 5,
    DP = 10,
    Costs = c(1, 1, 3, 5, 10, 20, 50),
    StepMort = 0.01
  ),
  Settlement = Settlement(FindMate = T)
)

par(mfrow=c(1,2))
plotProbs(demo@StageStruct)
plotProbs(disp@Emigration)

###########################

# Population is initialised in Patch 30:
init <- Initialise(InitType = 1,       # from loaded species distribution map
                   SpType = 0,         # all suitable cells
                   InitDens = 2,       # user-specified density
                   IndsHaCell = 10,
                   PropStages = c(0,0.5,0.5),
                   InitAge = 0)


neuTrait <- NeutralTraits(
  Positions = "random",
  NbOfPositions = 10,
  InitialDistribution = "uniform",
  InitialParameters = c("max" = 1), # two possible values, 0 and 1
  MutationDistribution = "KAM", # k-allele model, 
  MutationParameters = c("max" = 1),
  MutationRate = 0.05,
  OutputValues = TRUE
)

gen <- Genetics(
  GenomeSize = 50,
  Traits = Traits(Neutral = neuTrait),
  OutputStartGenetics = 30,
  OutputInterval = 5,
  PatchList = "random_occupied",
  NbrPatchesToSample = 10,
  nIndividualsToSample = 50,
  Stages = 1,
  # OutputFstatsWeirCockerham = TRUE #,
  OutputFstatsWeirHill = TRUE
)

num_replicates <- 20

sim <- Simulation(Simulation = 0, 
                  Replicates = num_replicates, 
                  Years = 100,
                  OutIntPop = 1,
                  OutIntOcc = 1,
                  OutIntRange = 1)

s <- RSsim(
  batchnum = 3,
  land = land,
  demog = demo,
  dispersal = disp,
  simul = sim,
  init = init,
  gene = gen,
  seed = 324135
)

RunRS(s, dirpath)


par(mfrow=c(1,2))
plotAbundance(s, dirpath)
plotOccupancy(s, dirpath)


# Some helper code to import the output files for plotting

# Read the first replicate output file text file into a dataframe.
# The other replicates will be appended to this dataframe in the for loop below.

NeutralGenetics_df <- read_delim(paste0(dirpath, "Outputs/Batch3_Sim0_Land1_Rep0_pairwisePatchNeutralGenetics.txt"), delim = "\t")

for(replicate in 1:(num_replicates-1)){
  
  temp_df <- read_delim(paste0(dirpath, "Outputs/Batch3_Sim0_Land1_Rep", replicate, "_pairwisePatchNeutralGenetics.txt"), delim = "\t")

  
  NeutralGenetics_df <- rbind(NeutralGenetics_df, temp_df)
  
}

NeutralGenetics_df_filter <- NeutralGenetics_df |> 
  filter(Fst != 0)

Fst_plot <- NeutralGenetics_df_filter |> 
  ggplot(aes(x = as.factor(Year), y = Fst)) +
  #geom_point(position = position_jitter(width = 0.1)) +
  geom_boxplot(outliers = FALSE) +
  labs(x = "Year") +
  theme_minimal()

Fst_plot

