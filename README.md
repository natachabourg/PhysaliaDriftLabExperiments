# PhysaliaDriftLabExperiments

This repository contains code supporting the publication titled "Ocean wanderers: a lab-based investigation into the effect of wind and morphology on the drift of _Physalia_ spp." by Natacha Bourg, Amandine Schaeffer, Anne Molcard, Christopher Luneau, Daniel E. Hewitt, Rémi Chemin.

## Overview

The repository includes:

- **Calibrate_Camera:** This directory contains all the codes used to create the calibration matrix required for correcting image distortion captured by the camera.

- **TrackAndCalibrateTrajectories.ipynb:** This Jupyter notebook automates the process of identifying the positions of _Physalia_ spp. models in the images and calibrating their trajectories to determine their true angle and speed. It utilizes the calibration matrix generated in the previous step.

- **PlotAnalysis.ipynb:** This notebook is dedicated to generating the three main plots analysing lab-results data presented in the paper.

- **Revision_Angle_Attack.ipynb:** Notebook dedicated to the computation of the angle of attack of our models in the lab. 

- **beach_survey_analysis:** This directory contains all the codes written by Daniel Hewitt (UNSW) to generate the figure analysing beach survey results presented in the paper.
