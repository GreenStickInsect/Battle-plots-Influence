## Authors:
## -  GreenStickInsect aka Ogrodnik10

## This file contains functions handling reading, generating and rearranging of battle data
# Do not run this file directly, instead, source() it.

# Please be aware, following functions are often not designed to fail gracefully if invalid data is supplied.
# Use with care.


# A decorator which guarantees that global R settings remain unchanged by the wrapped function,
# even if an error occurs in the course of its execution.
#
# Warning - admittedly I do not know how this happens, but calling a function wrapped by this decorator
# within another plotting function appears to impact the plotting within one of those.
# (I could not determine whether it is inner or outer)
# Perhaps wrapping multiple times is confusing R somehow, or I am just missing something.
# Instead, inside plotting functions you should call unwrapped versions of functions, and then
# wrap your own function.
#
# Arguments:
#   f - Function to wrap
#   fname - Name of function to use in error messages. Set to name of wrapper (output)
#           if you don't mind whoever attempts fixing the error to first stumble upon
#           this decorator. Set to name of wrapped (original) function to point them straight there.
#           Or, well, just write whatever, if you want chaotic and unhelpful error messages. :evil_smile:
#
#   returns: wrapper function.
ensure_isolation = function(f__, fname)
{
  wrapper = function(...)
  {
    # Save original settings
    p = par(no.readonly = TRUE) # No point trying to restore settings which cannot be edited anyway
    o = options()
    
    # Welcome to the world of forbidden arts and dark magic!
    #
    # This obscure thing ahead catches any error which might occur in wrapped function,
    # restores original settings, then changes the error message to
    # mislead any receiver of said message into thinking that
    # no wrapping or interceptions took place at all, (we need to stay in the shadows)
    # and finally, itself throws this masterpiece of deception.
    #
    # Oh, and in the boring case of no errors, just intercepts the result along
    # with info whether it was returned via invisible()
    tryCatch(
      {result = withVisible(f__(...))},
      error = function(e)
      {
        par(p)
        options(o)

        for (i in seq(length(e$call)))
        {
          if (e$call[i] == call("f__") & e$call[i+1] == call("..."))
          {
            e$call[i] = call(fname)
            arg = list(...)
            vals = vapply(arg, typeof, character(1), USE.NAMES=F)
            for (j in seq(length(arg)))
            {
              e$call[i+j] = do.call(call, list(vals[j]))
              names(e$call)[i+j] = names(arg)[j]
            }
          }
        }
        stop(e)
      }
    )
    
    # Restore original settings
    par(p)
    options(o)
    
    # Return result invisibly or not, depending on what the original function did
    if (result$visible) return(result$value)
    else return(invisible(result$value))
  }
  
  return(wrapper)
}

# Sorts rows of a data frame by numerical values in specified column.
#   data - data.frame to sort
#   by - numerical column by which to sort
#
#   returns: sorted data.frame.
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
#   ... - additional arguments to supply to strsplit().
#
#   returns: list of same length as x,
#            the i-th element of which contains the vector of splits of x[i],
#            excluding empty strings.
betterstrsplit = function(string, split, ...)
{
  x = strsplit(string, split, ...)
  
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
#   returns: list containing fields with info typically found in such files.
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


# A function taking in battle info (data) list and filling in (required) missing fields.
# Some fields will be guessed based on raw log of battle events, some filled in with placeholders.
# Vector with names of required fields can be specified.
#   battleinfo - original battleinfo list. (list)
#   required - (optional) vector with names of fields which should appear in output list. (defaults to all) (vector of character)
#   rawset - (optional) data.frame with raw battle events, as read from the .csv . If not supplied, some fields cannot be guessed and will remain NULL. (data.frame)
#   warn - (optional) whether to send warnings if a field cannot be guessed. (defaults to TRUE) (bool)
#
#   returns: a list.
prepare_battleinfo = function(battleinfo, required=c("all"), rawset=NULL, warn=TRUE)
{
  info = list()
  
  if ("all" %in% required)
  {
    required = c(required, "start", "end", "battle_length", "number", "place", "attacker",
                 "defender", "winner")
  }
  
  if ("start" %in% required | "battle_length" %in% required)
  {
    if ( all(is.null(rawset)) & warn) warning("Warning: prepare_battleinfo: `start` or `battle_length` is required but rawset was not supplied, might return NULL!")
    
    if (! is.null(battleinfo$start)) info$start = battleinfo$start
    else if (! all(is.null(rawset)) ) info$start = min(rawset$timestamp)
  }
  
  if ("end" %in% required | "battle_length" %in% required)
  {
    if ( all(is.null(rawset)) & warn ) warning("Warning: prepare_battleinfo: `end` or `battle_length` is required but rawset was not supplied, might return NULL!")
    
    if (! is.null(battleinfo$end)) info$end = battleinfo$end
    else if (! all(is.null(rawset)) ) info$end = max(rawset$timestamp)
  }
  
  if ("battle_length" %in% required)
  {
    if (! is.null(battleinfo$battle_length)) info$battle_length = battleinfo$battle_length
    else if (! (is.null(info$start) | is.null(info$end)) ) info$battle_length = ceiling((info$end - info$start) / 60 / 60)
  }
  
  if ("number" %in% required)
  {
    if ( all(is.null(rawset)) & warn ) warning("Warning: prepare_battleinfo: `number` is required but rawset was not supplied, might return NULL!")
    
    if (! is.null(battleinfo$number) ) info$number = battleinfo$number
    else if (! all(is.null(rawset)) ) info$number = rawset$battle[1]
  }
  
  if ("place" %in% required)
  {
    if ( all(is.null(rawset)) & warn ) warning("Warning: prepare_battleinfo: `place` is required but rawset was not supplied, might return NULL!")
    
    if (! is.null(battleinfo$place) ) info$place = battleinfo$place
    else if (! all(is.null(rawset)) )
    {
      # this monster returns number of occurrences of each land in the events
      t=table(vapply(strsplit(sets$ALL$raw$tileId, "-"), function(vect) {return(vect[1])}, c(""), USE.NAMES=F))
      
      info$place = names(t)[which(t == max(t))]
    }
    
    if (! is.null(info$place)) info$place = tools::toTitleCase(gsub("_", " ", info$place))
  }
  
  if ("attacker" %in% required)
  {
    if (! is.null(battleinfo$attacker) ) info$attacker = battleinfo$attacker
    else info$attacker = "XX" # There is no reliable way to guess that
  }
  
  if ("defender" %in% required)
  {
    if (! is.null(battleinfo$defender) ) info$defender = battleinfo$defender
    else info$defender = "XX" # There is no reliable way to guess that
  }
  
  if ("winner" %in% required)
  {
    if (! is.null(battleinfo$winner) ) info$winner = battleinfo$winner
    else info$winner = "XX" # There is no reliable way to guess that
  }
  
  return(info)
}


# Reads data from ip spend file and creates a dataset from it.
# Note: output dataset is missing a per_player field, create it manually with prepare_per_player
# Note #2: this function does not generate some of rows in the "raw" dataset, you need to create them
#          with add_convenience_columns
# Note #3: if data comes in an old format, some columns might be missing or renamed.
#          You can reformat the data after reading, with ensure_modern_format

# Dataset format is list of:
#   faction - name of team which this dataset belongs to
#   color - color of said team
#   raw - A data.frame with following columns: (these might vary depending on file contents)
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
#     mode - method in which IP was applied. Valid values are: "MANUAL", "DIRECT_PLAY", "PASSIVE_PLAY" (character)
#     sumUsed - total raw (before bonuses) IP used (sum of ipUsed and bipUsed) (numeric)
#     color - RGB (in hex) color of team the player belongs to (character)
#
#   per_player - A data.frame with following columns:
#     name - username (non-unique) of a player (character)
#     user - user id (unique) of a player (character)
#     ipUsed - raw (before bonuses) IP from playing Influence games (excluding DirectPlay feature) (numeric)
#     bipUsed - raw (before bonuses) BIP from Discord (numeric)
#     DPUsed - raw (before bonuses) IP from Direct Play feature (numeric)
#     PPUsed - raw (before bonuses) IP from Passive Play feature (numeric)
#     sumUsed - total raw (before bonuses) IP used (sum of ipUsed, DPUsed and bipUsed) (numeric)
#     ipApplied - total IP applied to tile (after bonuses) (numeric)
#     faction - name of team player belongs to (character)
#     color - RGB (in hex) color of team the player belongs to (character)
#     bot - Whether or not this user is an automatic bot (i.e. faction's automatic attack mechanism) (logical)
     

# Arguments:
#   filename - path to the file with data to be read.
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
  if (! is.null(df$timestamp)) df$timestamp = as.numeric(df$timestamp)
  df$ipUsed = as.numeric(df$ipUsed)
  df$bipUsed = as.numeric(df$bipUsed)
  df$ipApplied = as.numeric(df$ipApplied)
  if (! is.null(df$directPlay)) df$directPlay = df$directPlay == "true"
  
  rownames(df) = seq(1, length(df$name))
  
  newset$raw = df
  return(newset)
}


# The format of the input data has been changing in the past.
# To ensure backwards-compatibility, this tool converts old data formats
# to the modern one supported by other functions.
#   dataset - An unmodified dataset, as returned by prepare_someset (list). Note, that only the "raw" dataset
#             will be reformatted - if you already created some other data (e.g. per_player dataset) based on
#             it, it will not be updated and will need to be re-created.
#
#   returns: a list (reformatted dataset)
ensure_modern_format = function(dataset)
{
  raw = dataset$raw
  
  if ("myTeam" %in% colnames(raw)) # Old format with "myTeam" instead of "team"
  {
    colnames(raw)[colnames(raw) == "myTeam"] = "team"
  }
  
  if (! is.null(raw$directPlay)) # Old format with directPlay: true/false
  {
    raw$mode = ifelse(raw$directPlay, "DIRECT_PLAY", "MANUAL")
    raw = raw[- which(colnames(raw) == "directPlay")]
  } else if (is.null(raw$mode)) # Old format where only manual play mode existed
  {
    raw$mode = "MANUAL"
  }
  
  if (is.null(raw$timestamp)) # Old format without timestamps - note that some plots won't work without these
  {
    raw$timestamp = NULL
  }
  
  dataset$raw = raw
  return(dataset)
}


# Adds some more columns to the "raw" dataset, which technically do not contain data that
# could not be sourced otherwise, but expose it in a more convenient way.
# These columns are often needed by other functions, so this step of data preparation should not be skipped.
#   dataset - An unmodified dataset, as returned by prepare_someset or ensure_modern_format. (list)
#
#   returns: a list (expanded dataset)
add_convenience_columns = function(dataset)
{
  raw = dataset$raw
  
  colnames(raw)[colnames(raw) == "team"] = "faction"
  raw$faction = toupper(raw$faction)
  
  raw$color = unlist(faction_colors[raw$faction], use.names=FALSE)
  
  raw$sumUsed = raw$ipUsed + raw$bipUsed
  
  dataset$raw = raw
  return(dataset)
}


# Since prepare_someset, ensure_modern_format and add_convenience_columns should usually be ran in sequence,
# the following function does all that automatically, simplifying data preparation process.
#
#   filename - path to the file with data to be read.
#
#   returns: a list (dataset with fully prepared "raw" data. per_player is not yet included)
read_and_reformat_dataset = function(filename)
{
  return( add_convenience_columns( ensure_modern_format( prepare_someset(filename) ) ) )
}


# Creates a "per_player" data frame (listing summarical scores of each player in a dataset)
# See docstring of function "prepare_someset" for the format of such data frame
#   rawall - "raw" data.frame of a dataset (see docstring of function "prepare_someset" for the format of such data frame)
#
#   returns: a data.frame
prepare_per_player = function(rawall)
{
  per_player = data.frame(name="", user="", ipUsed=0, bipUsed=0, DPUsed=0, PPUsed=0,
                          sumUsed=0, ipApplied=0, faction="", color="", bot=FALSE)
  
  for(idx in seq(1, length(rawall$user)))
  {
    row = rawall[idx,]
    if (! row$user %in% per_player$user)
    {
      tmp = data.frame(name=row$name, user=row$user, ipUsed=0, bipUsed=row$bipUsed, DPUsed=0, PPUsed=0,
                       sumUsed=row$sumUsed, ipApplied=row$ipApplied, faction=row$faction, color=row$color, bot=F)

      tmp$ipUsed = ifelse(row$mode=="MANUAL" | row$mode=="AUTO", row$ipUsed, 0)
      tmp$DPUsed = ifelse(row$mode=="DIRECT_PLAY", row$ipUsed, 0)
      tmp$PPUsed = ifelse(row$mode=="PASSIVE_PLAY", row$ipUsed, 0)
      
      # A bot should ALWAYS have its actions marked as "AUTO", so this only needs to be checked once
      if (row$mode=="AUTO") {tmp$bot = TRUE}
      
      per_player = rbind(per_player, tmp)
    } else
    {
      idx = which(per_player$user == row$user)
      if (per_player[idx, "name"] != row$name) per_player[idx, "name"] = paste(row$name, "*")
      
      if (row$mode == "DIRECT_PLAY")
      {
        per_player[idx, "DPUsed"] = per_player[idx, "DPUsed"] + row$ipUsed
      } else if (row$mode == "MANUAL" | row$mode == "AUTO")
      {
        per_player[idx, "ipUsed"] = per_player[idx, "ipUsed"] + row$ipUsed
      } else if (row$mode == "PASSIVE_PLAY")
      {
        per_player[idx, "PPUsed"] = per_player[idx, "PPUsed"] + row$ipUsed
      } else
      {
        warning(paste0("Warning: prepare_per_player: Ignoring row with timestamp ", row$timestamp, " due to invalid play mode!"))
      }
      
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
                                        per_player = data.frame(name="", user="", ipUsed=0, bipUsed=0, DPUsed=0,
                                                                PPUsed=0, sumUsed=0, ipApplied=0, faction="",
                                                                color="", bot=F))
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