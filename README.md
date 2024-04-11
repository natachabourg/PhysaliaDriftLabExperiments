# PhysaliaDriftLabExperiments

This repository contains code supporting the publication titled "Ocean wanderers: lab-based investigation of the wind and morphology's control over \textit{Physalia physalis}' journey" by Natacha Bourg, Amandine Schaeffer, Anne Molcard, Christopher Luneau, Daniel E. Hewitt, Rémi Chemin.

## Overview

The repository includes:

- **Calibrate_Camera:** This directory contains all the codes used to create the calibration matrix required for correcting image distortion captured by the camera.

- **TrackAndCalibrateTrajectories.ipynb:** This Jupyter notebook automates the process of identifying the positions of Physalia models in the images and calibrating their trajectories to determine their true angle and speed. It utilizes the calibration matrix generated in the previous step.

- **PlotAnalysis.ipynb:** This notebook is dedicated to generating the three main plots analysing lab-results data presented in the paper.

- **beach_survey_analysis:** This directory contains all the codes written by Daniel Hewitt (UNSW) to generate the figure analysing beach survey results presented in the paper.

## How to Use

1. Navigate to the `Calibrate_Camera` directory to create the calibration matrix.
2. Execute the `TrackAndCalibrateTrajectories.ipynb` notebook to automatically identify Physalia positions and calibrate their trajectories.
3. Utilize the `PlotAnalysis.ipynb` notebook to generate the primary plots for analysis.

