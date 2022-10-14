# Instructions on how to update LMO tableau app.

1. Go to https://github.com/bcgov/LMO_tableau_R_code
2. Click the green code button.
3. Click the clipboard icon.
4. Start Rstudio>File>New Project>Version Control>Git
5. Control-V to paste the repository URL, choose a directory for the project on your hard drive (LAN too slow for this.)
6. Create Project
7. Create sub-directories raw_data and processed_data.
8. Get following files from 4CastViewer and put in folder raw_data.
    * Raw Emp single variables.csv (occBC4dei211JTT)
    * Raw JO single variables.csv (occBCjoi4d211JTT)
    * Raw DS single variables.csv (occBC4dsd211JTT)

9. ask "people" for current versions of the following files, and put in "raw_data"
      * "2021 Wages"
      * "lmo64_characteristics"
      * "NOC Mapping"
      * "Occupation Characteristics..."
      * "HOO list"
      * figure2_2...(a sheet from LMO Charts and Tables.xlsx)
      * "IndustryProfiles_Descriptions.xlsx" (direct input to tableau)
      * Definitions (direct input to tableau)
      * Education...(direct input to tableau)
      * supply-composition... (direct input to tableau)
10. Source file 01_source_me_process_data.R

      