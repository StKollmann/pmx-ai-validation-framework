# mlxtran Agent – Validation

Validation of an AI agent that generates mlxtran model files (Monolix/Simulx) from natural-language model descriptions.

## Method

- **Inputs**: 20 natural-language prompts covering PK, PKPD, viral dynamics, tumor growth inhibition, K-PD, neutrophils, exposure-response, metabolites, PD turnover and myelosuppression models (`ValidationDataSets/prompts_v*.csv`).
- **References**: one human-curated mlxtran model per prompt (`ValidationDataSets/references/`). Prompts 16 and 17 are two phrasings of the same model and share `Parent_3metabolites.txt`.
- **Generation**: each prompt was sent to the agent as a single-turn request, **10 replicates per prompt**, at several reasoning-effort settings (200 runs per model/effort).
- **Comparison** (`RScript/compare.R`): the generated and the reference model are both loaded into Simulx with the same dose (100 at t=0), the same random parameter values (`set.seed(123)`) and the same output time (t=12). Outputs are compared with R's `all.equal()` (default tolerance 1.5e-8). A run **passes only if every output variable matches**.

## Prompt versions

| Version | Change | Used by |
|---------|--------|---------|
| v1 | original prompts | GPT-5.6-sol, GPT-5.6-terra, Claude Opus 5 (`opus5v1_*`) |
| v2 | adds "Do not use odeType=stiff." to the 13 prompts whose reference does not use a stiff solver | Claude Opus 5 (`opus5v2_*`), Claude Sonnet 5 |

A stiff solver shifts results by ~1e-4, far above the comparison tolerance, so algebraically identical models failed on v1 when the model chose `odeType=stiff` unprompted. This mostly affected Claude Opus 5 (see `opus5v1_*` vs `opus5v2_*`). The GPT runs were not repeated on v2. **GPT and Claude results therefore come from different prompt versions.**

## Results

Pass rate over 200 runs (20 prompts × 10 replicates). "Excl. infra" leaves out runs that failed because of API errors or timeouts, where no model was produced. Full table: `Results/summary.csv`.

| Model | Prompts | Effort | Passed | Pass rate | Excl. infra | Mean gen. time |
|-------|:------:|--------|:------:|:---------:|:-----------:|:--------------:|
| gpt-5.6-sol | v1 | none | 188/200 | 94.0% | 94.0% | 11.4 s |
| gpt-5.6-sol | v1 | low | 195/200 | **97.5%** | 97.5% | 19.9 s |
| gpt-5.6-sol | v1 | medium | 191/200 | 95.5% | 95.5% | 15.6 s |
| gpt-5.6-terra | v1 | none | 143/200 | 71.5% | 71.5% | 13.3 s |
| gpt-5.6-terra | v1 | low | 180/200 | 90.0% | 90.0% | 15.7 s |
| gpt-5.6-terra | v1 | medium | 187/200 | 93.5% | 93.5% | 15.5 s |
| claude-opus-5 | v1 | low | 98/200 | 49.0% | 51.0% | 17.1 s |
| claude-opus-5 | v1 | medium | 83/200 | 41.5% | 41.5% | 21.5 s |
| claude-opus-5 | v2 | low | 164/200 | 82.0% | 82.0% | 16.3 s |
| claude-opus-5 | v2 | medium | 148/200 | 74.0% | 81.8% | 24.1 s |
| claude-sonnet-5 | v2 | low | 148/200 | 74.0% | 76.3% | 24.0 s |
| claude-sonnet-5 | v2 | medium | 169/200 | 84.5% | 84.5% | 24.0 s |

## Folder structure

```
mlxtranAgent/
├── ValidationDataSets/
│   ├── prompts_v1.csv           # Model type, Prompt, Reference file
│   ├── prompts_v2.csv
│   └── references/              # reference mlxtran models (<Reference file>.txt)
├── RScript/
│   ├── compare.R                # compares one generated model with one reference
│   └── ValidationScript.R       # re-scores all generated models of one or more runs
└── Results/
    ├── summary.csv              # pass rates per model / effort
    └── <model>_<effort>/
        ├── validation_report.csv            # per-run verdict, per-output details, error, generation time
        └── <Reference>_generated_rep<N>.txt # generated models
```

`validation_report.csv` columns: `Row` (prompt number), `Replicate`, `Model type`, `Reference`, `Pass`, `Details` (per-output `MATCH` or `all.equal()` difference), `Error`, `Time_s` (generation time).

Notes:
- A run folder holds 190 generated models, not 200: prompts 16 and 17 share a reference name, so the prompt 17 file overwrote prompt 16's. Their verdicts in `validation_report.csv` were scored before that happened and are complete.
- Runs that failed during generation have no model file.

## Reproducing the comparison

Requires MonolixSuite with `lixoftConnectors`.

```sh
# one pair
Rscript RScript/compare.R Results/gpt56sol_low/ER_Emax_generated_rep1.txt ValidationDataSets/references/ER_Emax.txt

# re-score whole runs (writes Results/<run>/results_rescored.csv)
cd RScript
Rscript ValidationScript.R gpt56sol_low opus5v2_low
```

`compare.R` prints `PASS=TRUE|FALSE` and `[1] TRUE|FALSE` on its last line. The generated model is the first argument, because its parameter list is used to draw the random parameter values.
