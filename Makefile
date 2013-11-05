.DEFAULT_GOAL := install

APP_FOLDER            = public/app

STYLES_VENDOR_DIR     = $(APP_FOLDER)/styles/vendor
BOOTSTRAP_CSS         = $(STYLES_VENDOR_DIR)/bootstrap.no-icons.min.css

FONT_AWESOME_DIR      = $(STYLES_VENDOR_DIR)/font-awesome
FONT_AWESOME_CSS      = $(FONT_AWESOME_DIR)/font-awesome.min.css
FONT_AWESOME_FONT_DIR = $(STYLES_VENDOR_DIR)/font
FONT_AWESOME_ZIP      = $(APP_FOLDER)/font-awesome.zip
FONT_AWESOME_TMP      = $(APP_FOLDER)/font-awesome-4.0.3

SCRIPTS_VENDOR_DIR    = $(APP_FOLDER)/scripts/vendor
ANGULAR               = $(SCRIPTS_VENDOR_DIR)/angular.min.js


install: backend webapp

backend: gratte-papier.cabal
	cabal install

gratte-papier.cabal: gratte-papier.cabal.m4
	m4 gratte-papier.cabal.m4 > gratte-papier.cabal

webapp: bootstrap $(ANGULAR) font_awesome

bootstrap: $(BOOTSTRAP_CSS)

$(BOOTSTRAP_CSS): | $(STYLES_VENDOR_DIR)
	wget 'http://netdna.bootstrapcdn.com/bootstrap/3.0.0/css/bootstrap.no-icons.min.css' -O $(BOOTSTRAP_CSS)

font_awesome:  $(FONT_AWESOME_FONT_DIR)/fontawesome-webfont.eot \
               $(FONT_AWESOME_FONT_DIR)/fontawesome-webfont.svg \
               $(FONT_AWESOME_FONT_DIR)/fontawesome-webfont.ttf \
               $(FONT_AWESOME_FONT_DIR)/fontawesome-webfont.woff \
               $(FONT_AWESOME_CSS) \
               clean_font_awesome

$(FONT_AWESOME_FONT_DIR)/fontawesome-webfont.%: | $(FONT_AWESOME_TMP) $(FONT_AWESOME_FONT_DIR)
	mv $(FONT_AWESOME_TMP)/fonts/$(notdir $@) $@

$(FONT_AWESOME_CSS): | $(FONT_AWESOME_TMP) $(FONT_AWESOME_DIR)
	mv $(FONT_AWESOME_TMP)/css/$(notdir $@) $@

$(FONT_AWESOME_TMP):
	wget 'http://fontawesome.io/assets/font-awesome-4.0.3.zip' -O $(FONT_AWESOME_ZIP)
	unzip $(FONT_AWESOME_ZIP) -d $(APP_FOLDER)

$(FONT_AWESOME_FONT_DIR):
	mkdir -p $(FONT_AWESOME_FONT_DIR)

$(FONT_AWESOME_DIR):
	mkdir -p $(FONT_AWESOME_DIR)

clean_font_awesome:
	rm -rf $(FONT_AWESOME_TMP)
	rm $(FONT_AWESOME_ZIP)

$(STYLES_VENDOR_DIR):
	mkdir -p $(STYLES_VENDOR_DIR)

$(ANGULAR): | $(SCRIPTS_VENDOR_DIR)
	wget 'https://ajax.googleapis.com/ajax/libs/angularjs/1.0.8/angular.min.js' -O $(ANGULAR)

$(SCRIPTS_VENDOR_DIR):
	mkdir -p $(SCRIPTS_VENDOR_DIR)
