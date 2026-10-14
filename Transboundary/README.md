# AB–NT Transboundary Reporting (Hay River and Slave River)

Scripts for the annual Alberta–NWT transboundary technical report: water-use triggers for the **Hay River**, plus hydroclimate figures and flow statistics for the **Hay** and **Slave** rivers.

> **If something fails, work through [Troubleshooting](#troubleshooting) before assuming the code is wrong.** Almost every error so far has been a missing file, a folder in the wrong place, or out-of-date climate data.

---

## What's in this folder

| File | What it is | Do you edit it? |
|---|---|---|
| `TB_Functions.R` | **Start here.** Loads everything, then makes the trigger plots and calculations for `report_year`. | Yes: `report_year` at the top |
| `TB_hydroclimate_plots.R` | Hydrometric and climate figures and flow statistics for the report. Run section by section. | Yes: `report_year` at the top (must match `TB_Functions.R`) |
| `tb_source_functions.R` | Loads functions from hydroclim, nwtclimate and `SOEfigs_functions.R`. `TB_Functions.R` runs it for you. | No |
| `tb_calc_triggers.R` | Function: monthly allocation and use as a percentage of natural border flow. | No |
| `tb_plot_triggers.R` | Function: plots the two trigger bar charts (calls `tb_calc_triggers`). | No |
| `data/<year>/` | AB licence and usage files for each year, plus the outputs written by the scripts. | Add each new year's files |
| `data/Figures and Tables/` | Where `TB_hydroclimate_plots.R` saves figures. | No |

`tb_calc_triggers.R` and `tb_plot_triggers.R` only *define* functions. Running them on their own does nothing visible, which is expected.

---

## One-time setup (new computer or new person)

The scripts expect this exact layout. Replace `<you>` with your Windows username (the folder name under `C:/Users/`).

```
C:/Users/<you>/Documents/
├── R_Scripts/
│   ├── Packages/
│   │   ├── hydroclim/        ← gnwt-operational branch (see step 1)
│   │   ├── nwtclimate/       ← gnwt-operational branch, INCLUDING the data/ folder
│   │   └── Transboundary/    ← this folder (all .R files + data/)
│   └── Functions/
│       └── Workflows/
│           ├── SOEfigs_functions.R
│           └── update_climate.R
```

1. **hydroclim and nwtclimate: download the `gnwt-operational` branch, not `main`.**
   Maude has restructured `main` of both repos. These scripts rely on files that only exist on the operational branch (for example `nwtclimate/R/dependencies_functions.R`, and `hydro_calc_dayofyear_cumulative.R` / `hydro_plot_dayofyear_cumulative.R` in hydroclim).
   - On GitHub, open the repo, click the branch dropdown (it says **main**), choose **gnwt-operational**, then **Code → Download ZIP**.
   - Unzip so the folder is named exactly `hydroclim` / `nwtclimate`, not `hydroclim-gnwt-operational`.
   - Repos: https://github.com/M-Auclair/hydroclim and https://github.com/M-Auclair/nwtclimate
2. **nwtclimate data files.** The ECCC `.rds` data files are in the `data/` folder of the nwtclimate gnwt-operational branch, so they come with the ZIP download. Check they are in `nwtclimate/data/`.
3. **Workflow scripts.** Download `SOEfigs_functions.R` and `update_climate.R` from https://github.com/EmmaGRiley/NWTHydroclimate and save them in `R_Scripts/Functions/Workflows/`.
4. **This folder.** Download the `Transboundary` folder from the same repo into `R_Scripts/Packages/Transboundary/`, including `data/`.
5. **Packages.** In RStudio:
   ```r
   install.packages(c("tidyhydat", "dplyr", "ggplot2", "lubridate", "foreign", "scales", "plotly", "magrittr"))
   tidyhydat::download_hydat()   # finalized WSC hydrometric data; re-run each year
   ```
   Also check the package list at the top of each hydroclim and nwtclimate script.

---

## Each year

### 1. Update climate data
Run `update_climate.R` (in `Functions/Workflows/`) to update the ECCC merged climate dataset. Then check that the `.rds` files in `nwtclimate/data/` have today's date. If you skip this, the climate plots silently stop at last year.

### 2. Update HYDAT
```r
tidyhydat::download_hydat()
```
HYDAT normally holds finalized data up to about 1–2 years ago. If the report year isn't in HYDAT yet, `hydro_calc_daily()` fills in with provisional real-time data (`tidyhydat::realtime_ws`). Real-time data only goes back about 18 months, so don't leave the report too long. Note in the report that the data are provisional.

### 3. Add this year's Alberta licence and usage files
Create `data/<report_year>/` (for example `data/2025/`) and save the AB files there **with exactly these names**. Replace `2025` with your year.

| File | Contents | Required? |
|---|---|---|
| `Hay_SW_LicenceDetail_2025.dbf` | Surface water licences in the Hay basin | If missing, 2022 licences are used as a proxy (a warning prints) |
| `Hay_GW_LicenceDetail_2025.dbf` | Groundwater licences | Same as above |
| `Hay_UsageDetail_2025.csv` | Reported monthly water use | If missing, only the allocation trigger (Trigger 1) is produced |

**Where these come from:** **[EMMA: confirm the source and contact. Who at Alberta EPA sends these, and is it a request each year or a download?]**

The usage file must have these columns (names as R reads them): `Water.Allocation.Id`, `Water.Allocation.Type` (`SW`/`GW`), `Measurement.Period.Date` (format like `31-Jan-25`), `Measurement.Value..m3.`. The licence `.dbf` files need `Water_Allo`, `CONSUMPTIV`, `LOSSES`, `SubClassif`. If AB changes their format, compare against `data/2022/`.

> 2023 and 2024 have no licence `.dbf` files, so those years used 2022 licences as a proxy. If newer licence files arrive, update the hard-coded `"2022/"` fallback in `tb_calc_triggers.R`.

### 4. Run the triggers
1. Open `TB_Functions.R` and set `report_year` at the top.
2. Click **Source**, or select all and press Ctrl+Enter.
3. Outputs:
   - `data/<year>/Hay _Trigger1_ <year> .png`: monthly allocation as % of natural border flow (dashed line = 2.5% trigger)
   - `data/<year>/Hay _Trigger2_ <year> .png`: monthly use estimate as % of natural flow (dashed = 4%, solid = 5%), only if a usage file exists
   - `data/<year>/Hay_SW_<year>.csv` and `Hay_GW_<year>.csv`: allocation summaries by sector
   - Console: *"Percentage of allocation to use for non reports: X%"*. This number goes in the report.

How it works (for reviewers):
- Natural border flow = gauge flow at 07OB001 × 0.94 (border scaling) + allocations (or use).
- Allocations are spread evenly by days in the month.
- Hutch Lake evaporation is the ENVIRONMENTAL surface-water allocation, distributed monthly using the Morton shallow-lake evaporation percentages for High Level.
- Non-reporting licences are assumed to use 2 × the reported use-to-allocation ratio.

### 5. Hydroclimate figures and statistics
1. Open `TB_hydroclimate_plots.R` and set `report_year` at the top. It must match `TB_Functions.R`.
2. Run `TB_Functions.R` first in the same session. It loads the functions and sets `file_path`.
3. Run `TB_hydroclimate_plots.R` **section by section**. Each block is one figure or statistic for the report:

| Section | Output |
|---|---|
| Hay River discharge | `hydro_plot_dayofyear()`: report year vs historic daily flow, 07OB001 |
| Cumulative rain | `clim_plot_dayofyear(cumulative = T)` for Hay River, High Level, Fort Nelson (Hay basin); Fort Smith, Fort Chipewyan, Mackenzie, Peace River (Slave basin) |
| Air temperature | Daily (`clim_plot_dayofyear`) and monthly mean (`clim_plot_monthly`) for 7 communities |
| Runoff trends | `YieldsTable()` and `SOEfigs_bystn()`. *Not normally used for the annual report.* |
| Total annual streamflow | Prints total annual discharge (m³), median ("normal") and % of mean for Hay and Slave |
| Peak and mean flows | Annual peak and mean annual flow, Hay River |
| Cumulative flow plots | Interactive (plotly) cumulative discharge, Hay and Slave |
| Slave consumptive-use threshold | `2 / (mean annual flow in km³)`. Should stay ~1.9% unless long-term mean flow changes significantly |

Notes:
- Slave River (07NB001) plots use `after_bennett = TRUE`, so the historic period starts after the W.A.C. Bennett Dam.
- Slave River 2020 total discharge is gap-filled with a fixed value (`142109496749` m³) from the 2020–2021 report.
- The "Other interactive plots" section at the bottom (Aklavik) isn't part of the TB report.

---

## Troubleshooting

| Message / symptom | Cause | Fix |
|---|---|---|
| `These scripts could not be found: ...` | hydroclim or nwtclimate is the `main` branch, or a folder is named wrongly (e.g. `hydroclim-main`) | Re-download the **gnwt-operational** branch, then rename the folder (Setup step 1) |
| `...SOEfigs_functions.R ... could not be found` | Workflow scripts not saved in `R_Scripts/Functions/Workflows/` | Setup step 3 |
| `Folder .../data/2025 does not exist` | This year's AB files not set up | Each year, step 3 |
| `cannot open file ... _LicenceDetail_2022.dbf` | No licence files for this year **and** the 2022 proxy files are missing | Copy `data/2022/` from GitHub |
| `object 'package_path' not found` / `object 'file_path' not found` | Ran a script before `TB_Functions.R` | Run `TB_Functions.R` first, in the same R session |
| `could not find function "hydro_calc_daily"` (or any `clim_`/`hydro_` function) | `tb_source_functions.R` didn't run, or the session was restarted | Re-run `TB_Functions.R` |
| Climate plots stop before the report year | ECCC data not updated | Each year, step 1 (`update_climate.R`) |
| Flow plots stop before the report year | Old HYDAT | `tidyhydat::download_hydat()` |
| Every path fails even though the files are there | Your Documents folder is redirected (e.g. OneDrive), so it isn't `C:/Users/<you>/Documents` | In R, run `path.expand("~")`. If it isn't `C:/Users/<you>/Documents`, ask IT, or change the `user <-` line in `tb_source_functions.R` and `TB_Functions.R` |
| Trigger 2 plot missing | No usage file this year | Expected. Only Trigger 1 is produced |

---

*Last updated October 2026 (Emma Gregory, before leave). Maintained by NT Hydrology, ECC.*
