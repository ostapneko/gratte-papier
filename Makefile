.DEFAULT_GOAL := all

APP_FOLDER            = public/app

STYLES_VENDOR_DIR     = $(APP_FOLDER)/styles/vendor
BOOTSTRAP_CSS         = $(STYLES_VENDOR_DIR)/bootstrap.no-icons.min.css

FONT_AWESOME_DIR      = $(STYLES_VENDOR_DIR)/font-awesome
FONT_AWESOME_CSS      = $(FONT_AWESOME_DIR)/font-awesome.min.css
FONT_AWESOME_FONT_DIR = $(STYLES_VENDOR_DIR)/fonts
FONT_AWESOME_ZIP      = /tmp/font-awesome.zip
FONT_AWESOME_TMP      = /tmp/Font-Awesome-4.0.3

SCRIPTS_DIR           = $(APP_FOLDER)/scripts
SCRIPTS_VENDOR_DIR    = $(SCRIPTS_DIR)/vendor
ANGULAR               = $(SCRIPTS_VENDOR_DIR)/angular.min.js

all: backend webapp

backend: gratte-papier.cabal
	cabal sandbox init
	cabal install unix-time
	cabal install --bindir=bin

gratte-papier.cabal: gratte-papier.cabal.m4
	m4 gratte-papier.cabal.m4 > gratte-papier.cabal


webapp: $(BOOTSTRAP_CSS) $(ANGULAR) font_awesome scripts

$(BOOTSTRAP_CSS): | $(STYLES_VENDOR_DIR)
	wget 'http://netdna.bootstrapcdn.com/bootstrap/3.0.0/css/bootstrap.no-icons.min.css' -O $(BOOTSTRAP_CSS)

font_awesome:  $(FONT_AWESOME_FONT_DIR)/fontawesome-webfont.eot \
               $(FONT_AWESOME_FONT_DIR)/fontawesome-webfont.svg \
               $(FONT_AWESOME_FONT_DIR)/fontawesome-webfont.ttf \
               $(FONT_AWESOME_FONT_DIR)/fontawesome-webfont.woff \
               $(FONT_AWESOME_CSS) \

$(FONT_AWESOME_FONT_DIR)/fontawesome-webfont.%: | $(FONT_AWESOME_TMP) $(FONT_AWESOME_FONT_DIR)
	cp $(FONT_AWESOME_TMP)/fonts/$(notdir $@) $@

$(FONT_AWESOME_CSS): | $(FONT_AWESOME_TMP) $(FONT_AWESOME_DIR)
	cp $(FONT_AWESOME_TMP)/css/$(notdir $@) $@

$(FONT_AWESOME_TMP): | $(FONT_AWESOME_ZIP)
	unzip $(FONT_AWESOME_ZIP) -d /tmp

$(FONT_AWESOME_ZIP):
	wget 'https://github.com/FortAwesome/Font-Awesome/archive/v4.0.3.zip' -O $(FONT_AWESOME_ZIP)

$(FONT_AWESOME_FONT_DIR):
	mkdir -p $(FONT_AWESOME_FONT_DIR)

$(FONT_AWESOME_DIR):
	mkdir -p $(FONT_AWESOME_DIR)

$(STYLES_VENDOR_DIR):
	mkdir -p $(STYLES_VENDOR_DIR)

$(ANGULAR): | $(SCRIPTS_VENDOR_DIR)
	wget 'https://ajax.googleapis.com/ajax/libs/angularjs/1.2.0/angular.min.js' -O $(ANGULAR)

$(SCRIPTS_VENDOR_DIR):
	mkdir -p $(SCRIPTS_VENDOR_DIR)

scripts:
	coffee -c $(SCRIPTS_DIR)
