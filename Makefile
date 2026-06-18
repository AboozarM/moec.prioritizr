all: clean docs test check

clean:
	rm -rf man/*.Rd

docs: man readme site

man:
	R --slave -e "devtools::document()"

readme:
	R --slave -e "rmarkdown::render('README.Rmd')"

purl_readme:
	R --slave -e "knitr::purl('README.Rmd', 'README.R')"
	rm -f Rplots.pdf

quicksite:
	R --slave -e "options(rmarkdown.html_vignette.check_title = FALSE);pkgdown::build_site(run_dont_run = TRUE, lazy = TRUE)"

site:
	R --slave -e "pkgdown::clean_site(force = TRUE)"
	R --slave -e "options(rmarkdown.html_vignette.check_title = FALSE);pkgdown::build_site(run_dont_run = TRUE, lazy = FALSE)"

test:
	R --slave -e "devtools::test()" > test.log 2>&1
	rm -f tests/testthat/Rplots.pdf

quickcheck:
	echo "\n===== R CMD CHECK =====\n" > check.log 2>&1
	R --slave -e "devtools::check(build_args = '--no-build-vignettes', args = '--no-build-vignettes', run_dont_test = TRUE, vignettes = FALSE)" >> check.log 2>&1

check:
	echo "\n===== R CMD CHECK =====\n" > check.log 2>&1
	R --slave -e "devtools::check(remote = TRUE, build_args = '--no-build-vignettes', args = '--no-build-vignettes', run_dont_test = TRUE, vignettes = FALSE)" >> check.log 2>&1

gpcheck:
	echo "\n===== GOOD PRACTICE =====\n" > gp.log 2>&1
	R --slave -e "goodpractice::gp('.')" >> gp.log 2>&1

wbcheck:
	R --slave -e "devtools::check_win_devel()"

jhwbcheck:
	R --slave -e "devtools::check_win_devel(email = 'jeffrey.hanson@uqconnect.edu.au')"

spellcheck:
	R --slave -e "devtools::document();devtools::spell_check()"

urlcheck:
	R --slave -e "devtools::document();urlchecker::url_check()"

build:
	R --slave -e "devtools::build()"

install:
	R --slave -e "devtools::install_local(force = TRUE)"

install_deps:
	R --slave -e "remotes::install_deps(dep = TRUE)"
	R --slave -e "remotes::install_deps(dep = 'Config/Needs/website')"

examples:
	R --slave -e "devtools::run_examples(run_donttest = TRUE, run_dontrun = TRUE);warnings()" > examples.log 2>&1
	rm -f Rplots.pdf

examples_cran:
	R --slave -e "devtools::run_examples();warnings()" > examples.log 2>&1
	rm -f Rplots.pdf

search_errors:
	@grep -rRnF --exclude="*.md" --exclude="*.R" --exclude=".Rd" --exclude="Makefile" --exclude="*.yaml" --exclude="*.js" --exclude="*.map" --exclude="*.json" --exclude="*.o" --exclude="*.so" --exclude-dir=".git" "Error"

update_standalone:
	R --slave -e "usethis::use_standalone('prioritizr/prioritizr', file = 'standalone-cli.R')"
	R --slave -e "usethis::use_standalone('prioritizr/prioritizr', file = 'standalone-assertions_handlers.R')"
	R --slave -e "usethis::use_standalone('prioritizr/prioritizr', file = 'standalone-assertions_class.R')"
	R --slave -e "usethis::use_standalone('prioritizr/prioritizr', file = 'standalone-assertions_functions.R')"
	R --slave -e "usethis::use_standalone('prioritizr/prioritizr', file = 'standalone-assertions_misc.R')"

.PHONY: clean docs readme site test check checkwb build install man spellcheck examples urlcheck search_errors
