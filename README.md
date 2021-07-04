# Tapered Scoring

This repository analyzes the tab data from HWS USUDC 2020 to see the effect of tapered scoring on the break. This README summarizes the process and results; for more details on the analysis, read `analysis.md`.

## Data

I collected the draw from every round of USUDC by downloading each draw's webpage as a PDF, then extracting the draw in a tidy, tabular form from the PDFs. (Some portion of the PDF tables were inaccessible within R, so I supplemented this with the OCR software Tabula.) All this raw data is contained in the  `raw_data` directory. In the script `create_results.R`, I combined these tables to create the clean dataset `final_data/results.csv`.

In addition to the draw, I downloaded the tournament break and extracted it to `final_data/breaks.csv`.

## Results

My analysis has two results:

1. Each round had no statistically significant effect on a team's probability of breaking.
2. Judge quality in round 1 does not distort the break.

Each of these is expanded on in `analysis.md`. To inspect the code and analysis itself, see `analysis.Rmd`.
