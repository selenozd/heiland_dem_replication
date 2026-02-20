# heiland_dem_replication

Replication package for "Extreme Weather and Mortality of Vulnerable Urban 
Populations: An Examination of Temperature and Unclaimed Deaths in New York 
City", forthcoming in *Demography* (2026).

**Authors:** Frank W. Heiland, Selen Ozdogan, Deborah Balk, 
Jennifer Brite, and Peter Marcotullio 

---

## Software Requirements

- R version 4.5.2 (2025-10-31), "Not Part in a Rumble"

---

## Directory Structure
```
heiland_dem_replication/
├── code/       # R scripts, numbered in order of execution
├── data/       # Input and use data
└── output/     # Tables and figures
```

---

## Replication Instructions

Run the scripts in the `code/` folder in the order indicated by their 
numbering. Data sources and required packages are documented within each 
script.

---

## Data Availability

The mortality records used in this study were scraped from the Hart Island 
Lookup Service, a public website maintained by the NYC Department of 
Correction. Since our data collection in Fall 2025, the City of New York 
has replaced this website with a new platform called Hart Island Loved One 
Lookup. The scraping script provided in this replication package targets the 
previous version of the website and will need to be revised to function with 
the current platform. We include it here for transparency and to document our 
original data collection methodology, which may be useful for researchers 
seeking to understand our approach or adapt it for similar purposes. 
This study also uses publicly available climate data from the Copernicus 
Climate Change Service, accessible via DOI 10.24381/cds.adbb2d47, and the 
National Centers for Environmental Information (NCEI), accessible via DOI 
10.1175/JTECH-D-11-00103.1. The authors had no special access privileges 
to any of the data.

---

## License

Code is released under the [BSD 3-Clause License](LICENSE).
