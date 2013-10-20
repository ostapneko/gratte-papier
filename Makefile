.DEFAULT_GOAL := install

APP_FOLDER = public/app
BOOTSTRAP_CSS = $(APP_FOLDER)/styles/vendor/bootstrap.min.css
BOOTSTRAP_THEME = $(APP_FOLDER)/styles/vendor/bootstrap-theme.min.css
SCRIPTS_VENDOR_DIR = $(APP_FOLDER)/scripts/vendor
BOOTSTRAP_JS = $(SCRIPTS_VENDOR_DIR)/bootstrap.min.js
JQUERY = $(SCRIPTS_VENDOR_DIR)/jquery.min.js

install: gratte-papier.cabal install_angular
	cabal install

gratte-papier.cabal: gratte-papier.cabal.m4
	m4 gratte-papier.cabal.m4 > gratte-papier.cabal

install_angular: install_bootstrap

install_bootstrap: $(BOOTSTRAP_CSS) $(BOOTSTRAP_JS)

$(BOOTSTRAP_CSS):
	mkdir -p $(APP_FOLDER)/styles/vendor && \
	wget 'http://netdna.bootstrapcdn.com/bootstrap/3.0.0/css/bootstrap.min.css' -O $(BOOTSTRAP_CSS)

$(BOOTSTRAP_JS): $(JQUERY) | $(SCRIPTS_VENDOR_DIR)
	wget 'http://netdna.bootstrapcdn.com/bootstrap/3.0.0/js/bootstrap.min.js' -O $(BOOTSTRAP_JS)

$(JQUERY): | $(SCRIPTS_VENDOR_DIR)
	wget 'http://code.jquery.com/jquery-1.10.1.min.js' -O $(JQUERY)

$(SCRIPTS_VENDOR_DIR):
	mkdir -p $(SCRIPTS_VENDOR_DIR)
