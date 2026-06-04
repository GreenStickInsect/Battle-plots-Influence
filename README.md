# Battle plots Influence
An [R programming language](<https://www.r-project.org/>) application for visualization of battle results from [Influence mobile game](<https://play.google.com/store/apps/details?id=com.teremok.influence>) (battles are currently in open beta).

**Dependencies:**
- R
- RStudio
  - For automated plotting with `plots_via_rstudio.R`. If you wish to do things manually or create your own app based on this one, you don't need it.
- (optional) [ggplot2](<https://ggplot2.tidyverse.org/>)
  - While this is optional, some advanced/prettier graphs (including animated) will not be available without it.
- (optional) [gganimate](<https://gganimate.com/>)
  - Optional, but animated plots will not be available without it.

**To quickly get the app running:**
- [Install R](<https://cran.r-project.org/mirrors.html>)
- [Install RStudio](<https://posit.co/download/rstudio-desktop/>) - for now the only (easy) way to run the app is through it. I *might* add a command-line interface in future.
- (optional) Install [ggplot2](https://ggplot2.tidyverse.org/) by running `install.packages("ggplot2")` (well, or some other way, if you have a preferred method).
- (optional) Install [gganimate](<https://gganimate.com/>) by running `install.packages("gganimate")`.
- Clone the repository to your computer - best done via command-line with **[git](<https://git-scm.com/>)**, but as simple as this repo is, you can also just click download on the files you need.
- Create a folder with battle data somewhere on your device.
  - battle data includes a CSV file with ip spend data and a file containing copy of general battle data (posted on Discord along every data set). You might need to create the latter manually.
    - While general battle data file is technically not required, many plots will be less accurate or even won't work without the information contained inside.
    - There is an example in folder [battle_xxx](</battle_xxx>).
  - If you create the folder within repo, it is recommended to use names such as "battle_???" since these are covered by `.gitignore`.
- Open the source file `plots_via_rstudio.R` in RStudio (`File -> Open File...`).
- Head to the 'Manually changed variables' section and set the control variables as follows:
  - `directory` - path to the directory with battle data (the one you created a moment ago).
  - `filenames` - vector of names of files with battle data (you will probably only have one, `ip_spend_battle_???.csv`.
  - `battleinfofile` - name of an additional file containing general battle data.
  - `export` - (`TRUE` or `FALSE`) whether plots should be exported as files (these appear in your data folder)
  - There are also other control variables documented in the code, allowing you to further customize the output.

- Run all the code (shortcut in RStudio - `Ctrl+A` (select all), `Ctrl+Enter` (run all selected, or current line))

<details>
    <summary>A small troubleshooting note: ⌄</summary>
        Sometimes output devices randomly get messed up and you might see your plots not drawn at all,
        or stubbornly drawn into some file when you don't tell them to.<br>
        (I don't think this is an issue with my code, either something with RStudio or R itself)<br><br>
        When this happens, run command 'dev.off() a few times, until it says 'null device'.
        This should close all output devices and let a default one open on next plot.
</details>

If you need help, best contact `@Ogrodnik10` aka `@greenstickinsect` on [Influence Discord server](<https://discord.gg/6yEf3cBq98>).
<br/>There is even a dedicated post for this project [here](<https://canary.discord.com/channels/562910943848169472/1418601973543862303>) (make sure you join the server before following this link, else it won't work).

Alternatively, you can also send an email to gstickinsect@gmail.com or [create an issue](<https://github.com/GreenStickInsect/Battle-plots-Influence/issues/new>) under this repo.
