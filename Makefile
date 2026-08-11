.PHONY: document test check bundle

document:
	RENV_CONFIG_SANDBOX_ENABLED=FALSE Rscript -e 'roxygen2::roxygenise()'

test:
	RENV_CONFIG_SANDBOX_ENABLED=FALSE Rscript -e 'testthat::test_local(reporter = "summary")'

check: document
	RENV_CONFIG_SANDBOX_ENABLED=FALSE R CMD build --no-manual .
	RENV_CONFIG_SANDBOX_ENABLED=FALSE _R_CHECK_FORCE_SUGGESTS_=false R CMD check --no-manual reactextract_0.1.4.tar.gz

bundle: check
	Rscript --vanilla scripts/build_offline_bundle.R
