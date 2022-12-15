[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](<Redirect-URL>)


# Instructions on how to update LMO tableau app.

1.  Go to <https://github.com/bcgov/LMO_tableau_R_code>

2.  Click the green code button.

3.  Click the clipboard icon.

4.  Start Rstudio\>File\>New Project\>Version Control\>Git

5.  Control-V to paste the repository URL, choose a directory for the project on your local drive (LAN too slow)

6.  Create Project

7.  Create sub-directories raw_data and processed_data.

8.  Get following files from 4CastViewer and place in raw_data.

    -   Raw Emp single variables.csv (occBC4dei211JTT) Select All occupations, All industries, All Characteristics (only Employment), All areas (8 areas selected), and CSV: Single Variables.

    -   Raw JO single variables.csv (occBCjoi4d211JTT). Select All occupations, All industries, All Characteristics (5 variables), All areas (8 areas selected) and CSV: Single Variables.

    -   Raw DS single variables.csv (occBC4dsd211JTT). Select All occupations, All industries, All Characteristics, All areas (5 areas) and CSV: Single Variables.

9.  ask Feng or Nicole for current versions of the following files, place in raw_data.

    -   "20xx Wages"
    -   "lmo64_characteristics"
    -   "NOC Mapping"
    -   "Occupation Characteristics..."
    -   "HOO list"
    -   figure2_2...(a sheet from LMO Charts and Tables.xlsx)
    -   "IndustryProfiles_Descriptions.xlsx" (direct input to tableau)
    -   Definitions (direct input to tableau)
    -   Education...(direct input to tableau)
    -   supply-composition... (direct input to tableau)

10. Source file 01_source_me_process_data.R (saves output to processed_data)

    -   If you have to make changes to the R code... pull, commit, push. (to push to github you will need a [pat](https://rfortherestofus.com/2021/02/how-to-use-git-github-with-r/))

11. Optional:

    -   Knit file 02_knit_me_summarize_data.Rmd (broad overview of input and output files and program flow)
    -   Knit file 03_knit_me_verify_inputs.Rmd (this requires last year's dataframes be available in a subfolder old_inputs)

12. Retrieve last year's tableau file from Y:\Labour Economics\BC Labour Market Outlook\\20xx Edition (20xx-20xx)\Tableau\LMO\_tableau_tool_20xx.twbx and place in project directory. (where xx is last year)

13. Open tableau, grab a coffee.

14. Click on data, and then for each of the listed data sources click update data source. (sometimes doesnt work first time, but then does???)

15. Once you have updated all the data sources click refresh all extracts (just to check)

16. Take a look through the tableau file... when you see a blank screen most likely due to with filter and/or dates not being correct

17. Once everything is working copy the whole directory over to the LAN.
