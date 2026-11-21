# Psych 251 PS3: Reproducibility checks

| Folder | Contents |
|---|---|
| [`partA/`](partA/) | Part A (known case): Joel, Teper & MacDonald (2014), Study 1. Report [`ps3_check_template_parta.qmd`](partA/ps3_check_template_parta.qmd), the course's `reproducibilityReport.Rmd`, and the Study 1 data. |
| [`partB/`](partB/) | Part B (a classmate's project paper). |
| [`transcripts/`](transcripts/) | Exported agent transcripts, one per part. |

## Published reports

- Part A: https://connect.posit.cloud/limgabs/content/01a1ffad-9212-b622-4e32-5584515d3371
- Part B: to come

## Rendering and publishing

Run from inside each part's folder, for example:

```bash
cd partA
quarto publish posit-connect-cloud ps3_check_template_parta.qmd
```

Each part's `_publish.yml` keeps updates going to the same URL.
