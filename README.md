# CO2 Storage Analysis Tool 

![R](https://img.shields.io/badge/Language-R-blue) ![Shiny](https://img.shields.io/badge/Framework-Shiny-blue) ![Status](https://img.shields.io/badge/Status-Completed-success)

## Overview
This repository contains the CO2 Storage Analysis Tool, an interactive software developed using R Studio and R Shiny. The tool is designed to evaluate the feasibility of depleted gas reservoirs for Carbon Capture and Storage (CCS).

The project focuses on a specific case study:
* Sink (Storage): Beckler-008 Depleted Gas Reservoir (Cooper/Eromanga Basin, South Australia).
* Source (Emissions): Major natural gas-fired power plants in the CALABARZON region, Philippines (Ilijan, Sta. Rita, and San Lorenzo).

## Key Features
The application provides a comprehensive workflow for CCS assessment:

1.  Reservoir Suitability Classification: an initial screening module based on Raza et al. (2016) criteria (Depth, Porosity, Permeability, etc.).
2.  Capacity Calculation (OGIP Method): Calculates theoretical and usable CO2 storage capacity using the Volumetric Equation.
3.  Emissions Forecasting: Projects future CO2 emissions using linear regression based on historical data (1994–2023).
4.  Reservoir Simulation: Visualizes the fill-up process over time, modeling pressure changes using the Soave-Redlich-Kwong (SRK) Equation of State.
5.  Sensitivity Analysis: A Spider Plot tool to rank which geological parameters like Porosity and Bgi, most significantly impact storage capacity.

## Data & Methodology
### Data Sources
* Reservoir Data: Geological parameters such as porosity, permeability, net thickness sourced from the Beckler-008 Well Completion Report.
* Emissions Data: Historical CO2 emissions (1994–2023) from the Department of Energy (DOE) for Ilijan, Sta. Rita, and San Lorenzo plants.

### Key Findings
Based on the analysis performed by this tool:
* Usable Capacity: The Beckler-008 reservoir has an estimated usable capacity of ~145,935 kt CO2e (after applying a 10% safety factor).
* Storage Lifespan: The reservoir can accommodate the projected emissions from the three power plants for approximately 45.5 years (starting from 2025).
* Sensitivity: The Gas Formation Volume Factor (Bgi) was identified as the most influential parameter on capacity estimation.

## Installation & Usage
To run this application locally, you need R and RStudio installed.

1.  Clone the repository:
    ```bash
    git clone [https://github.com/your-username/CO2-Storage-Analysis-Tool-Beckler008.git](https://github.com/your-username/CO2-Storage-Analysis-Tool-Beckler008.git)
    ```
2.  Install Required Libraries:
    Open `Beckler 008 Final.R` and ensure the following packages are installed:
    ```r
    install.packages(c("shiny", "shinydashboard", "readxl", "dplyr", "tidyr", 
                       "ggplot2", "plotly", "shinyjs", "scales", "shinyWidgets", 
                       "RColorBrewer", "rlang", "stringr", "purrr", "glue", 
                       "zoo", "DT", "phaseR"))
    ```
3.  Run the App:
    Open the R script and click the **"Run App"** button in RStudio.

## Authors
* Kimm Z. Alvarez
* Oliven Diandly L. Cosep
* Lloyd Roldan A. Mejorada
* Benchz Nicole C. Sobrepeña (Leader)

*Bachelor of Science in Petroleum Engineering, Palawan State University (2025)*

## License
This project is for educational and research purposes.
