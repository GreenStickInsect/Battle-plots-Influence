## Authors:
## -  GreenStickInsect aka Ogrodnik10


### Source the functions ###

# Set work dir to current script location.
# Careful - this might not work if run otherwise than from within RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# source data processing functions
source("data_functions.R")

# source plotting functions
source("plotting_functions.R")

### Manually changed variables ###

# Change as needed - this is a directory in which data files are and where plots will be generated
directory = "./battle_125"

# File with data
filenames = c("ip_spend_battle_125.csv")
battleinfofile = "battleinfo.txt"

# whether to export to files
#   FALSE - draw the plots within R
#   TRUE - instead, draw the plots as .png files
#
# Note: file names are defined in code below, separately for each plot type
# They can be changed manually
export = FALSE


### Preparations ###
setwd(directory)

# RGB (in hex) colors of each team
faction_colors = data.frame(CI="#1aa44f", MT="#fc8c36", DC="#3f5fde",
                            PS="#beff64", BB="#ff496c", FG="#0099ff",
                            CO="#9c6aff", CR="#b20000", NO_TEAM="#303030")


### Generate datasets ###

## Try to obtain battle info
# Data from older battles does not have this, so it is fault-proof
if (file.access(battleinfofile)[1] == 0)
{
  battleinfo = read_battle_data("battleinfo.txt") # see data processing source file
} else
{
  battleinfo = list(number=NA_integer_, place=NA_character_, attacker=NA_character_, defender=NA_character_,
                    winner=NA_character_, start=NA_integer_, end=NA_integer_)
}

## If some of data is missing, try to guess it
# WARNING: battle length is then set arbitrarily. Old battles always lasted 48-49 hours,
#          but if you have data from battle which lasted different amount of time AND does not
#          have battle info file - you need to tweak this manually.
if ( !(is.null(battleinfo$start) | !is.na(battleinfo$start)) &&
     !(is.null(battleinfo$end) | !is.na(battleinfo$end)))
{
  battleinfo$battle_length = ceiling((battleinfo$end - battleinfo$start) / 60 / 60) # hours
} else
{
  battleinfo$battle_length = 49 # hours
}

## Temporary list of datasets generated from each input file.
# This allows for reading data that is split across multiple files.
nonesets = list(null=list())
for (i in seq(length(filenames)))
{
  tmp = prepare_someset(filenames[i])
  nonesets[[paste("None",i,sep="")]] = tmp
}
rm(i, tmp) # clean up
nonesets = nonesets[-1]

## Join the temporary datasets into one big and remove the temporary list

# This already follows the following dataset format:
#   List of:
#     raw - data frame with raw data (as read from CSV, every IP input event separately)
#     faction - name of team which this dataset belongs to
#     color - color of the above team
#     per_player - (not yet included here) data frame with total scores of each player
allset = list(raw=nonesets[[1]]$raw, faction="All", color="gray60")
for (i in seq(length(nonesets)))
{
  if (i == 1) next
  allset$raw = rbind(allset$raw, nonesets[[i]]$raw)
}

# Create a list of datasets, containing the "ALL" dataset created above
sets = list(ALL=allset)
rm(i, nonesets, allset) # clean up

# Generate the missing per_player data frame and add to "ALL" dataset
sets$ALL$per_player = prepare_per_player(sets$ALL$raw)

# Generate separate dataset for every team and add those to list of datasets
# Note: those datasets are stored in the list under name corresponding to its team's (short) name
# e.g. sets$PS is dataset documenting performance of Protectores Silva
# You can look up known faction names in "faction_colors" constant defined above
sets = append(sets, prepare_factionsets(sets$ALL))


### Drawing the plots ###

## Per player plots

# This includes "All Players" plot as well as "Per faction" plots
# Basically just plot scores of every player in the dataset
for (s in sets)
{
  text.cex = 1
  if (s$faction == "All") text.cex = 0.8
  
  tofile = FALSE
  if (export) tofile = paste("Plot_players_", s$faction, ".png", sep="")
  player_plot(s, text.cex=text.cex, cex.names=0.8, tofile=tofile)
}
rm(s, text.cex) # cleanup

## Contribution plot

# Which teams did every player help
if (! export) {contribution_plot(sets$ALL)
} else contribution_plot(sets$ALL, tofile="Contribplot.png")

## Bonuses plot

# How high bonuses each player utilized.
# Be warned: this just divides "ipApplied" column by sum of
# "Used" columns (in fact, datasets already have a "sumUsed" column defined)
if (! export) {bonusplot(sets$ALL)
} else bonusplot(sets$ALL, tofile="Bonusplot.png")

## Scaleplot

# Divides players into 2 teams, where 1st is minimal number of highest-scoring players,
# whose scores sum up to same or higher score as those of all other players.
#
# Internally utilizes the "vsplot" function.
if (! export) {scaleplot(sets$ALL)
} else scaleplot(sets$ALL, tofile="Scaleplot.png")

## Sourceplot

# Compares IP, DirectPlay and Discord BIP usage across whole battle.
#
# Internally utilizes the "vsplot" function.
if (! export) {sourceplot(sets$ALL)
} else sourceplot(sets$ALL, tofile="Sourceplot.png")

## Timeline

# Visualizes performance of each team over time
if (! export) {timeline(sets, battleinfo)
} else timeline(sets, battleinfo, tofile="Timeline.png")

## Custom vsplot

# This is a manually composed plot comparing total scores of customized teams.
# For usage reference and tips, see the file with plotting functions

# if (! export) {vsfile = FALSE
# } else vsfile = "VSplot.png"
# vsplot(teams=list(deltans=rbind(sets$BB$per_player, sets$DC$per_player), protectors=sets$PS$per_player),
#        names=c("Deltans", "Protectors"), tofile=vsfile)

export = FALSE # Make sure this remains false after execution, we don't want to generate files by accident