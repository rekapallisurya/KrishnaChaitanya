# Dataset Description

This project simulates a synthetic General Liability insurance portfolio suitable for actuarial pricing and reserving exercises.

## Data files

1. policies.csv
- Core policy attributes and rating factors.
- Includes NAICS segment, payroll, prior claims, and latent risk factors.

2. exposures.csv
- Policy-year exposure records.
- Policies can span multiple years with changing exposure and payroll.

3. claims.csv
- Transaction-level claims with accident year, reporting delay, claim type, and ultimate loss.

4. claim_development.csv
- Long-tail claim development triangle in long format.
- Includes cumulative and incremental paid by accident year and development age.

## Intended use

- Frequency and severity modelling
- Pure premium modelling
- Claims reserving diagnostics
- Explainable pricing analysis
