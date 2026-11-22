# Psych 251 PS3: Reproducibility checks

| Folder | Contents |
|---|---|
| [`partA/`](partA/) | Part A (known case): Joel, Teper & MacDonald (2014), Study 1. Report [`ps3_check_template_parta.qmd`](partA/ps3_check_template_parta.qmd), the course's `reproducibilityReport.Rmd`, and the Study 1 data. |
| [`partB/`](partB/) | Part B (a classmate's project paper): Yin, Jia & Wakslak (2024). Report [`ps3_check_template_partb.qmd`](partB/ps3_check_template_partb.qmd), the paper and SI, the classmate's replication report, and the authors' OSF data, code and preregistrations in `data/`. |
| [`transcripts/`](transcripts/) | Exported agent transcripts, one per part. |

## Published reports

- Part A: https://connect.posit.cloud/limgabs/content/01a1ffad-9212-b622-4e32-5584515d3371
- Part B: https://connect.posit.cloud/limgabs/content/01a20167-60ae-9eb7-0c7e-264fd3f60ac8

## Rendering and publishing

Run from inside each part's folder, for example:

```bash
cd partA
quarto publish posit-connect-cloud ps3_check_template_parta.qmd
```

Each part's `_publish.yml` keeps updates going to the same URL.
