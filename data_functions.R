## Authors:
## -  GreenStickInsect aka Ogrodnik10

## This file contains functions handling reading, generating and rearranging of battle data
# Do not run this file directly, instead, source() it.

# Please be aware, following functions are often not designed to fail gracefully if invalid data is supplied.
# Use with care.


# Sorts rows of a data frame by numerical values in specified column.
#   data - data.frame to sort
#   by - numerical column by which to sort
#
#   returns: sorted data.frame
datasort = function(data, by="sumUsed")
{
  data = data[order(data[,by], decreasing=TRUE), ]
  rownames(data) = seq(1, length(data[,by]))
  
  return(data)
}

# Improved version of R's built-in "strsplit" function
# Ensures that data produced from splitting does not contain empty strings
#   string - character vector to split
#   split - character vector containing regular expression(s) to use for splitting.
#
#   returns: list of same length as x,
#            the i-th element of which contains the vector of splits of x[i],
#            excluding empty strings
betterstrsplit = function(string, split)
{
  x = strsplit(string, split)
  
  for (i in seq(length(x)))
  {
    x[[i]] = as.character(na.omit(ifelse(x[[i]] == "", NA, x[[i]])))
  }
  
  return(x)
}

# Reads battle data file.
# (the one containing general info such as battle number, start time, etc.)
#   filename - path to the file to be read
#
#   returns: list containing fields with info typically found in such files
read_battle_data = function(filename)
{
  data = readChar(filename, file.info(filename)$size)
  
  data = unlist(betterstrsplit(data, "\n"))
  
  battledata = list(number=NA_integer_, place=NA_character_, attacker=NA_character_, defender=NA_character_,
                    winner=NA_character_, start=NA_integer_, end=NA_integer_)
  
  for (line in data)
  {
    pair = unlist(strsplit(line, ": "))
    
    if (length(pair) >= 2 &&
        trimws(pair[1]) %in% names(battledata))
    {
      idx = match(trimws(pair[1]), names(battledata))
      battledata[[idx]] = as(trimws(pair[2]), typeof(battledata[[idx]]))
    }
  }
  
  return(battledata)
}

# Reads data from ip spend file and creates a dataset from it.
# Dataset format is list of:
#   faction - name of team which this dataset belongs to
#   color - color of the above team
#   raw - A data.frame with following columns:
#     battle - battle number (numeric)
#     timestamp - Unix timestamp of this event (numeric)
#     name - username (non-unique) of player who triggered this event (character)
#     user - user id (unique) of player who triggered this event (character)
#     faction - name of team player belongs to (character)
#     action - "ATTACK" or "DEFEND" depending on whether player supported attacking or defending team (character)
#     tileid - ID of the tile event occured on (character)
#     attacker - name of the attacking team, or "null" if this tile was not being attacked (character)
#     defender - name of the defending team (character)
#     ipUsed - raw (before bonuses) IP from playing Influence games (numeric)
#     bipUsed - raw (before bonuses) BIP from Discord (numeric)
#     ipApplied - total IP applied to tile (after bonuses) (numeric)
#     directPlay - whether the listed value of ipUsed comes from DirectPlay feature (logical)
#     sumUsed - total raw (before bonuses) IP used (sum of ipUsed and bipUsed) (numeric)
#     color - RGB (in hex) color of team the player belongs to (character)
#
#   per_player - A data.frame with following columns:
#     name - username (non-unique) of a player (character)
#     user - user id (unique) of a player (character)
#     ipUsed - raw (before bonuses) IP from playing Influence games (excluding DirectPlay feature) (numeric)
#     bipUsed - raw (before bonuses) BIP from Discord (numeric)
#     DPUsed - raw (before bonuses) IP from DirectPlay feature (numeric)
#     sumUsed - total raw (before bonuses) IP used (sum of ipUsed, DPUsed and bipUsed) (numeric)
#     ipApplied - total IP applied to tile (after bonuses) (numeric)
#     faction - name of team player belongs to (character)
#     color - RGB (in hex) color of team the player belongs to (character)
#     
#
# Arguments:
#   filename - path to the file to be read
#
#   returns: a list

prepare_someset = function(filename)
{
  data = read.csv(filename, header=TRUE, sep=",")
  
  # Leftover from code handling data in format used after battle 107.
  # This code is currently not reverse-compatible with format of that one battle.
  # ...maybe it will be, one day...
  
  #pathparts = unlist(strsplit(filename, "/"))
  #fnameparts = unlist(strsplit(pathparts[length(pathparts)], "_"))
  
  newset = list(faction="None", color="white")
  
  df = data.frame(data)
  df$battle = as.numeric(df$battle)
  df$timestamp = as.numeric(df$timestamp)
  df$ipUsed = as.numeric(df$ipUsed)
  df$bipUsed = as.numeric(df$bipUsed)
  df$sumUsed = df$ipUsed + df$bipUsed
  df$ipApplied = as.numeric(df$ipApplied)
  df$directPlay = df$directPlay == "true"
  
  if ("myTeam" %in% colnames(df))
  {
    colnames(df)[colnames(df) == "myTeam"] = "faction"
  } else
  {
    colnames(df)[colnames(df) == "team"] = "faction"
  }
  df$faction = toupper(df$faction)
  
  df$color = unlist(faction_colors[toupper(df$faction)], use.names=FALSE)
  rownames(df) = seq(1, length(df$name))
  
  newset$raw = df
  return(newset)
}

# Creates a "per_player" data frame (listing summarical scores of each player in a dataset)
# See docstring of function "prepare_someset" for the format of such data frame
#   rawall - "raw" data.frame of a dataset (see docstring of function "prepare_someset" for the format of such data frame)
#
#   returns: a data.frame
prepare_per_player = function(rawall)
{
  per_player = data.frame(name="", user="", ipUsed=0, bipUsed=0, DPUsed=0, sumUsed=0, ipApplied=0, faction="", color="")
  
  for(idx in seq(1, length(rawall$user)))
  {
    row = rawall[idx,]
    if (! row$user %in% per_player$user)
    {
      tmp = data.frame(a=row$name, b=row$user, c=0, d=row$bipUsed, e=0, f=row$sumUsed, g=row$ipApplied, h=row$faction, i=row$color)
      tmp$c = ifelse(row$directPlay, 0, row$ipUsed)
      tmp$e = ifelse(row$directPlay, row$ipUsed, 0)
      
      names(tmp) = names(per_player)
      per_player = rbind(per_player, tmp)
    } else
    {
      idx = which(per_player$user == row$user)
      if (per_player[idx, "name"] != row$name) per_player[idx, "name"] = paste(row$name, "*")
      
      if (row$directPlay)
      {
        per_player[idx, "DPUsed"] = per_player[idx, "DPUsed"] + row$ipUsed
      } else per_player[idx, "ipUsed"] = per_player[idx, "ipUsed"] + row$ipUsed
      
      per_player[idx, "bipUsed"] = per_player[idx, "bipUsed"] + row$bipUsed
      per_player[idx, "sumUsed"] = per_player[idx, "sumUsed"] + row$sumUsed
      per_player[idx, "ipApplied"] = per_player[idx, "ipApplied"] + row$ipApplied
    }
  }
  per_player = per_player[-1,]
  per_player = datasort(per_player)
  
  return(per_player)
}

# Splits data from an "ALL" dataset into multiple sets, one for each team
#   allset - an "ALL" dataset (list)
#
#   returns: list of lists (datasets)
prepare_factionsets = function(allset)
{
  rawall = allset$raw
  factionsets = list(none=list())
  
  for (idx in seq(1, length(rawall$name)))
  {
    row = rawall[idx,]
    if (! row$faction %in% names(factionsets))
    {
      factionsets[[row$faction]] = list(raw=row, faction=row$faction, color=row$color,
                                        per_player = data.frame(name="", user="", ipUsed=0, bipUsed=0, DPUsed=0, sumUsed=0, ipApplied=0, faction="", color=""))
    } else
    {
      factionsets[[row$faction]]$raw = rbind(factionsets[[row$faction]]$raw, row)
    }
  }
  
  pplayerall = allset$per_player
  for (idx in seq(1, length(pplayerall$user)))
  {
    row = pplayerall[idx,]
    factionsets[[row$faction]]$per_player = rbind(factionsets[[row$faction]]$per_player, row)
  }
  
  # Remove dummy set
  factionsets = factionsets[-1]
  
  # Remove 1st, dummy rows from per_player dataframes
  for (setname in names(factionsets)) factionsets[[setname]]$per_player = factionsets[[setname]]$per_player[-1,]
  
  # Set every data frames' rownames to sequence of numbers going up from 1
  for (setname in names(factionsets))
  {
    rownames(factionsets[[setname]]$per_player) = seq(1, length(factionsets[[setname]]$per_player$name))
    rownames(factionsets[[setname]]$raw) = seq(1, length(factionsets[[setname]]$raw$name))
  }
  
  return(factionsets)
}