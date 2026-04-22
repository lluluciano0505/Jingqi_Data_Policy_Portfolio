# MUSA 5080 Data Policy Portfolio

This repository contains Luciano Lu's portfolio for MUSA 5080 (Public Policy Analytics), built and published as a Quarto static website.

## Project Overview

This portfolio showcases coursework analyses and project deliverables, including:

- Assignment 1: Census Data Quality for Policy Decisions
- Assignment 2: Healthcare Access and Equity in Pennsylvania
- Midterm: Philadelphia Housing Price Prediction Model
- Assignment 4: Spatial Predictive Modeling of Sanitation 311 Requests
- Assignment 5: Space-Time Prediction of Bike Share Demand (Indego)
- Final: Fire Alarms Analysis
- Midterm and Final Presentation Slides

Site entry points and navigation are defined in [_quarto.yml](_quarto.yml) and [index.qmd](index.qmd).

## Repository Structure

- [index.qmd](index.qmd): Homepage
- [_quarto.yml](_quarto.yml): Quarto site configuration (navbar, theme, output directory)
- [assignments/](assignments): Assignment, midterm, and final project pages
- [weekly-notes/](weekly-notes): Weekly reflection notes
- [Labs/](Labs): Lab materials
- [docs/](docs): Rendered static site for GitHub Pages

## Local Development

### 1. Prerequisites

- Install [Quarto](https://quarto.org/docs/get-started/)
- Install [R](https://cran.r-project.org/) (this project is primarily R/Quarto-based)
- Optional for VS Code language support: install the R package `languageserver`

### 2. Preview the site

From the repository root:

```bash
quarto preview
```

### 3. Render the site

```bash
quarto render
```

Rendered output is written to [docs/](docs), as configured by `output-dir: docs` in [_quarto.yml](_quarto.yml).

## GitHub Pages Deployment

1. Push the latest changes to GitHub.
2. In repository Settings -> Pages, set:
	- Branch: `main`
	- Folder: `/docs`
3. Save and wait for the site to publish.

## Common Maintenance Tasks

- Add a weekly note: create `week-XX-notes.qmd` in [weekly-notes/](weekly-notes)
- Add an assignment page: add a `.qmd` file under [assignments/](assignments), then update `navbar` in [_quarto.yml](_quarto.yml)
- Change site theme: edit `format.html.theme` in [_quarto.yml](_quarto.yml)

## Notes

This repository is for coursework and portfolio presentation. Please cite data sources and external materials in the corresponding assignment files.

