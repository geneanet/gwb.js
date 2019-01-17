BUILD_DIR=_build/

$(BUILD_DIR):
	mkdir $@

CORDOVA_BUILD_DIR=$(BUILD_DIR)cordova/
CORDOVA=cd $(CORDOVA_BUILD_DIR) && cordova

ELECTRON_BUILD_DIR=$(BUILD_DIR)electron/
ELECTRON=cd $(ELECTRON_BUILD_DIR) && electron-forge

DUNE=cd src/ml && dune
RSYNC=rsync --recursive --ignore-times

JSNAME=gwb.bc.js
JSFILE=src/ml/_build/default/gwb.bc.js
APPNAME=gwb

src/ml/templates.ml: $(wildcard src/templates/*.jinja2)
	ocaml src/templates/compile.ml src/templates/ > $@

$(JSFILE): src/ml/templates.ml $(wildcard src/ml/*.ml)
	$(DUNE) build --profile release $(JSNAME)

build: $(JSFILE)

cordova-init: | $(BUILD_DIR)
	cordova create $(APPNAME) && mv $(APPNAME) $(CORDOVA_BUILD_DIR)
	$(CORDOVA) platform add browser
	$(CORDOVA) platform add android

cordova-build: build | $(BUILD_DIR)
	cp $(JSFILE) src/cordova/www/
	$(RSYNC) src/cordova/ $(CORDOVA_BUILD_DIR)
	$(CORDOVA) build browser
	$(CORDOVA) build android

 cordova-run-android: | $(BUILD_DIR)
	$(CORDOVA) run --nobuild android

cordova-run-browser:
	BROWSER='chromium-browser --disable-web-security' $(CORDOVA) run --nobuild browser

cordova-clean:
	$(CORDOVA) clean browser
	$(CORDOVA) clean android

$(ELECTRON_BUILD_DIR) electron-init: $(BUILD_DIR)
	electron-forge init $(APPNAME) && mv $(APPNAME) $(ELECTRON_BUILD_DIR)

electron-build: $(ELECTRON_BUILD_DIR)
	cp $(JSFILE) src/electron/src/
	$(RSYNC) src/electron/ $(ELECTRON_BUILD_DIR)

electron-run:
	$(ELECTRON) start

clean:
	$(DUNE) clean

mr-proper: clean
	rm -rf $(CORDOVA_BUILD_DIR)
	rm -rf $(ELECTRON_BUILD_DIR)
