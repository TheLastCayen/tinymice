CC=lazbuild
OBJ = ptinymice.lpi
DATADIR = /usr/share
BINDIR = /usr/bin

tinymice: $(OBJ)
	$(CC) $^
#	ifeq (, $(shell which upx))
#	ifneq ("$(wildcard /usr/bin/upx)","")
#	upx --best ./bin/$@
#	endif

.PHONY: clean

clean:
	rm -Rf ./lib
	rm -Rf /Cross.Codebot-master/source/lib
	rm -Rf ./libs/mouseandkeyinput/lib
	rm -Rf ./bin

.PHONY: install
install: 
	mkdir -p $(DATADIR)/tinymice/languages/
	cp ./bin/tinymice $(DATADIR)/tinymice/
	cp ./languages/* $(DATADIR)/tinymice/languages/
	cp ./pictures/tinymice.png $(DATADIR)/icons/hicolor/128x128/apps/tinymice.png

	chown -R root:root $(DATADIR)/tinymice
	chmod -R 644 $(DATADIR)/tinymice/languages/*
	chmod 755 $(DATADIR)/tinymice/tinymice
	
	chown root:root $(DATADIR)/icons/hicolor/128x128/apps/tinymice.png
	chmod 644 $(DATADIR)/icons/hicolor/128x128/apps/tinymice.png

	ln -s $(DATADIR)/icons/hicolor/128x128/apps/tinymice.png $(DATADIR)/pixmaps/tinymice.png
	ln -s $(DATADIR)/tinymice/tinymice $(BINDIR)/tinymice
	chown root:root $(DATADIR)/pixmaps/tinymice.png
	chown root:root $(BINDIR)/tinymice

	echo "[Desktop Entry]" > $(DATADIR)/applications/TinyMouse.desktop
	echo "Name=TinyMice" >> $(DATADIR)/applications/TinyMouse.desktop
	echo "Comment=Auto Clicker" >> $(DATADIR)/applications/TinyMouse.desktop
	echo "Exec=tinymice" >> $(DATADIR)/applications/TinyMouse.desktop
	echo "Icon=tinymice" >> $(DATADIR)/applications/TinyMouse.desktop
	echo "Terminal=false" >> $(DATADIR)/applications/TinyMouse.desktop
	echo "Type=Application" >> $(DATADIR)/applications/TinyMouse.desktop
	echo "Categories=Utility;" >> $(DATADIR)/applications/TinyMouse.desktop
	echo "StartupNotify=true" >> $(DATADIR)/applications/TinyMouse.desktop
	echo "Keywords=Auto;Clicker;AutoClicker;Auto Clicker;Mouse;" >> $(DATADIR)/applications/TinyMouse.desktop

	chown root:root $(DATADIR)/applications/TinyMouse.desktop
	chmod 755 $(DATADIR)/applications/TinyMouse.desktop


.PHONY: uninstall
uninstall:
	rm -f $(DATADIR)/applications/TinyMouse.desktop
	rm -f $(DATADIR)/pixmaps/tinymice.png
	rm -f $(BINDIR)/tinymice
	rm -f $(DATADIR)/icons/hicolor/128x128/apps/tinymice.png
	rm -Rf $(DATADIR)/tinymice
