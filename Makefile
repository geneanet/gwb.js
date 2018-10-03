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
	$(DUNE) build $(JSNAME)
	sed -i 's/function fs_node_supported()/function fs_node_supported(){return false}function _FOOOOOOOOOOOOOOOOO()/g' $(JSFILE)

build: $(JSFILE)

cordova-init: | $(BUILD_DIR)
	cordova create $(APPNAME) && mv $(APPNAME) $(CORDOVA_BUILD_DIR)
	rm -rf $(CORDOVA_BUILD_DIR)www/img
	rm -rf $(CORDOVA_BUILD_DIR)www/css
	rm -rf $(CORDOVA_BUILD_DIR)www/js
	rm $(CORDOVA_BUILD_DIR)www/index.html
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
	$(CORDOVA) run --nobuild browser

cordova-rm:
	rm -rf $(CORDOVA_BUILD_DIR)

electron-init:
	electron-forge init $(APPNAME) && mv $(APPNAME) $(ELECTRON_BUILD_DIR)
	rm -rf $(ELECTRON_BUILD_DIR).git
	rm $(ELECTRON_BUILD_DIR).gitignore
	rm $(ELECTRON_BUILD_DIR)src/index.html
	rm $(ELECTRON_BUILD_DIR)src/index.js

electron-build:
	cp $(JSFILE) src/electron/src/
	$(RSYNC) src/electron/ $(ELECTRON_BUILD_DIR)

electron-run:
	$(ELECTRON) start

electron-rm:
	rm -rf $(ELECTRON_BUILD_DIR)

clean:
	$(DUNE) clean

mr-proper: clean electron-rm cordova-rm
