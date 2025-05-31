# Study or Party

## Overview

**Study or Party** is an R-based interactive tool designed to predict students' final academic performance using multiple linear regression. By inputting various student characteristics and behaviors, users can forecast academic outcomes and understand the key factors influencing student performance. It specifically helps visualize the balance between academic dedication and social activities.

## Features

* **Data Preprocessing**: Cleans and filters student dataset based on relevant criteria (e.g., attendance, school, academic goals).
* **Multiple Linear Regression Model**: Predicts the final grade (G3) based on demographic data, previous grades, and behavioral factors.
* **Interactive User Interface**: Provides two modes—Detailed and Simplified—for users to input student data for predictions.
* **Key Factor Identification**: Highlights the top 10 most influential variables affecting academic outcomes.
* **Visual Analysis**: Generates scatter plots comparing predicted vs actual student performance.

## Usage

### Prerequisites

* R (version ≥ 4.0 recommended)
* R packages:

  ```R
  install.packages(c("dplyr", "ggplot2"))
  ```

### Running the Script

1. Clone the repository:

```bash
git clone https://github.com/jamie950315/Study_or_Party.git
cd Study_or_Party
```

2. Execute the script:

```bash
Rscript Study_or_Party.r
```

3. Follow on-screen prompts to:

   * Choose **Detailed** (input all variables) or **Simplified** mode (top 10 influential variables).
   * Input student characteristics to receive a predicted final grade.

## Data Source

The model uses the [Student Performance Dataset](https://archive.ics.uci.edu/dataset/320/student+performance) (student-mat.csv), capturing various student demographics, academic history, and behaviors.

## Interpretation

Upon running the prediction, the script provides:

* Predicted final grade (G3).
* Insights into which variables significantly impact student performance.

Use the insights to adjust variables, exploring scenarios like increasing study time or reducing social activities to observe their impact on predicted academic outcomes.

## Applications

Ideal for:

* Educational data analytics teaching.
* Academic performance analysis for educators and students.
* Decision-support scenarios to balance academics and social life effectively.

## License

This project is open-source, distributed under the MIT License.
