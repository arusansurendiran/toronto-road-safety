# Starter folder

## Overview

We investigate the geographic, temporal and behavioural factors contributing to vehicle collisions across Toronto from 2006 to 2026. Spatial and temporal analyses reveal disparities in collision risk across Toronto regions, alongside peaks during commuter hours and late weekend nights. Furthermore, fatalities are highest among collisions with aggressive driving and heavy truck involvement, putting pedestrians as the most vulnerable group. While current municipal policies align with a decline in Toronto collisions, the study emphasizes needing targeted, data-informed interventions to address spatial and behavioural risks.

## File Structure

The repo is structured as:

-   `data/01-raw_data` contains the raw data as obtained from the City of Toronto.
-   `data/02-analysis_data` contains the cleaned dataset that was constructed.
-   `scripts` contains the R scripts used to download, clean and analyze data.
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper. 
-   `other` contains select papers in `literature` and details about LLM chat interactions in `llm_usage`.

## Statement on LLM usage

Aspects of the code were written with the help of Google Gemini. The chat history is available in inputs/llms/usage.txt.

