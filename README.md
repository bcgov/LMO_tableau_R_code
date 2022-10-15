# Instructions on how to update LMO tableau app.

1.  Go to <https://github.com/bcgov/LMO_tableau_R_code>

2.  Click the green code button.

3.  Click the clipboard icon.

4.  Start Rstudio\>File\>New Project\>Version Control\>Git

5.  Control-V to paste the repository URL, choose a directory for the project on your local drive (LAN too slow)

6.  Create Project

7.  Create sub-directories raw_data and processed_data.

8.  Get following files from 4CastViewer and put in folder raw_data.

    -   Raw Emp single variables.csv (occBC4dei211JTT)
    -   Raw JO single variables.csv (occBCjoi4d211JTT)
    -   Raw DS single variables.csv (occBC4dsd211JTT)

9.  ask Feng or Nicole for current versions of the following files, place in "raw_data"

    -   "2021 Wages"
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

12. Retrieve tableau file from Y:\Labour Economics\BC Labour Market Outlook\\2022 Edition (2022-2032)\Tableau\LMO\_tableau_tool_2022.twbx and place in directory.

13. Open tableau, grab a coffee.

14. Click on data, and then for each of the listed data sources click update data source. (sometimes you will need to be persistant.)

15. Once all the data sources have been updated click refresh all extracts for good measure.

16. Take a look through the tableau file. Most problems will be associated with filter and dates not being correct (if all data has been filtered out you just get a blank screen.)

17. Once everything is working transfer the whole directory over to the LAN.
