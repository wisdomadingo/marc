# convertGeneList.R

convertGeneList <- function(genes, from_species, to_species, verbose = FALSE) {
  # Ensure the biomaRt package is available
  if (!requireNamespace("biomaRt", quietly = TRUE)) {
    stop("Package 'biomaRt' is required but not installed.")
  }
  
  # Lookup list mapping species names to their Ensembl dataset and gene attribute
  species_info <- list(
    human = list(dataset = "hsapiens_gene_ensembl", attribute = "hgnc_symbol"),
    mouse = list(dataset = "mmusculus_gene_ensembl", attribute = "mgi_symbol"),
    rat   = list(dataset = "rnorvegicus_gene_ensembl", attribute = "rgd_symbol")
    # Add more species as needed.
  )
  
  # Check that both source and target species are supported
  if (!from_species %in% names(species_info)) {
    stop(paste("Unsupported from_species:", from_species))
  }
  if (!to_species %in% names(species_info)) {
    stop(paste("Unsupported to_species:", to_species))
  }
  
  # Extract dataset and attribute names for both species
  from_dataset   <- species_info[[from_species]]$dataset
  from_attribute <- species_info[[from_species]]$attribute
  to_dataset     <- species_info[[to_species]]$dataset
  to_attribute   <- species_info[[to_species]]$attribute
  
  # Connect to Ensembl databases for the source and target species
  mart_from <- tryCatch(
    biomaRt::useMart("ensembl", dataset = from_dataset),
    error = function(e) stop("Error connecting to source mart: ", e)
  )
  mart_to <- tryCatch(
    biomaRt::useMart("ensembl", dataset = to_dataset),
    error = function(e) stop("Error connecting to target mart: ", e)
  )
  
  # Retrieve the mapping using the getLDS function
  mapping <- tryCatch(
    biomaRt::getLDS(attributes = c(from_attribute),
                    filters = from_attribute,
                    values = genes,
                    mart = mart_from,
                    attributesL = c(to_attribute),
                    martL = mart_to,
                    uniqueRows = TRUE),
    error = function(e) stop("Error retrieving mapping: ", e)
  )
  
  if (nrow(mapping) == 0) {
    warning("No mapping found for the provided genes.")
    return(character(0))
  }
  
  # Extract unique target species gene symbols (from the second column)
  result <- unique(mapping[, 2])
  
  # Optionally print the first few mapped gene symbols
  if (verbose) {
    print(head(result))
  }
  
  return(result)
}
