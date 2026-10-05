# Re-export the saved participation summaries

This fixes the layout of the previous export. It **does not read the database
again**, change your installed package, or update the fake-data generator.

## What you need

- Your existing reactextract installation (0.5.4 or later 0.5.x with the rc14 dictionary).
- The original `participation-v6-INTERNAL/conditional-profile-INTERNAL.rds`, still
  inside the enclave. This contains the saved aggregate summaries.
- This complete new kit, unzipped into a **separate folder** in the enclave.

No new R packages, internet connection or Oracle connection are needed.
Do not overwrite the original profile kit or delete its saved files.

## Run it

1. Open RStudio inside the enclave.
2. Open `01_reexport.R` from this new kit.
3. Select **Session → Set Working Directory → To Source File Location**.
4. Run:

   ```r
   source("01_reexport.R")
   ```

5. A file picker opens. Select **`conditional-profile-INTERNAL.rds`** from the
   original kit's **`participation-v6-INTERNAL`** folder. Do not select the
   previous exported CSV folder, a round checkpoint, or respondent data.
6. Wait for the public-domain checks and the 25 round messages. The file can
   take a little time to load. Everything uses saved summaries; there are no
   new database queries.

The new folder will be **`react-participation-conditional-v2-candidate`**.
The script refuses to overwrite it. If interrupted, remove an incomplete output
only after checking it, or use a fresh copy of this code-only kit in another
folder. You can select the same original checkpoint again.

## Check and return

Open **`AVAILABILITY.md`** in the new candidate folder. It reports whether the
participation and age counts needed next are visible. It does not reveal hidden
counts. Some other distributions may still need the documented public fallback.

Ask for the usual enclave disclosure review of the **entire new candidate
folder**, using `REVIEWER.md` in this kit. Earlier profile and diagnostic releases,
including the previous conditional candidate, must be considered together.
Technical checks and a good availability report are not disclosure approval.

Once transfer is permitted, copy back **only**
`react-participation-conditional-v2-candidate`. If the availability report flags
problems, return that protected report with the candidate for review; **do not
repeat the database pull or change the privacy settings**.

Never transfer `participation-v6-INTERNAL`, the original RDS file, or an R
workspace (`.RData`). Unsuppressed aggregate data must stay in the enclave.
Do not zip the whole original kit: it now contains returned results.

## What changed

- One shared non-response count and one count for everyone else per round.
  “Everyone else” includes partial and uncertain respondents; it does not mean
  confirmed survey completion.
- NHS-derived age is a single distribution, not a table comparing age with itself.
- Governed individual-questionnaire fields are profiled among people **not**
  classified as shared nonrespondents. Registration, independent fields and
  documented exceptions are not given a blanket shared-nonresponse overwrite.
- The already reviewed REACT-1 adult smoking/vaping fields use NHS-age 18+
  only. Missing age does not count as adult. Other incomplete compound routes
  are not silently treated as solved.
- `AGE` remains separate from `U_AGE`, with its NHS-age context retained.
- Other answer profiles pool age groups. This candidate does **not** release
  or claim an empirical age–participation relationship. Using its separate age
  and participation components without another reviewed link would assume
  independence within each round. That limitation is recorded for integration.
- No overlapping alternative version of a field distribution is exported.
  Detailed stage and age comparisons remain saved inside the enclave.

All original suppression rules are retained: counts below 10 (including zero),
additional hidden cells to protect them, linked row/column protections where
applicable, and rounding visible counts to five. The export is smaller because
unneeded subdivisions are combined **before** those same protections run.

This is still a candidate, not the final approved profile v6 or reactextract 0.6.0.

## Without a file picker (optional)

```r
source("00_load.R")
internal_aggregates <- readRDS("PATH/TO/participation-v6-INTERNAL/conditional-profile-INTERNAL.rds")
compact_candidate <- pr6_reexport(internal_aggregates)
pr6_write(compact_candidate, "react-participation-conditional-v2-candidate")
```

Replace only `PATH/TO` with the original location inside the enclave. Do not use
the returned, suppressed CSVs as input. Hidden counts cannot be reconstructed
from them.
