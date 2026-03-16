source("scripts/data_generation/generate_policies.R")
source("scripts/data_generation/generate_exposures.R")
source("scripts/data_generation/generate_claims.R")
source("scripts/data_generation/generate_claim_triangle.R")

project_root <- getwd()
data_dir <- file.path(project_root, "data")
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

policies <- generate_policies(output_path = file.path(data_dir, "policies.csv"))
exposures <- generate_exposures(policies, output_path = file.path(data_dir, "exposures.csv"))
claims <- generate_claims(policies, exposures, output_path = file.path(data_dir, "claims.csv"))
triangle <- generate_claim_triangle(claims, output_path = file.path(data_dir, "claim_development.csv"))

message("Data generation complete")
message("Policies: ", nrow(policies))
message("Exposures: ", nrow(exposures))
message("Claims: ", nrow(claims))
message("Triangle rows: ", nrow(triangle))
