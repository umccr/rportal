roxydoc:
	@R -e "devtools::document()" --quiet --no-restore --no-save

build:
	@R -e "pak::local_install(upgrade = FALSE)" --quiet --no-restore --no-save

check:
	@R -e "devtools::check()" --quiet --no-restore --no-save
