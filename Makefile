HAKYLL = $(shell pwd)/result/bin/newartisans

all: build
	echo Newartisans.com is built

build:
	nix-build '<nixpkgs>' -A haskell801Packages.newartisans

site:
	$(HAKYLL) rebuild

watch:
	$(HAKYLL) watch

deploy: site
	@echo Copying files...
	rsync --checksum -av --delete _site/ jw:/srv/newartisans/

	@echo Setting ownership...
	ssh jw chown -R nginx:nginx /srv/newartisans

	@echo Setting permissions...
	ssh jw chmod -R ugo+rX /srv/newartisans

	@echo Setting contexts...
	ssh jw chcon -R -u system_u -t httpd_sys_content_t /srv/newartisans

	@echo Restarting nginx...
	ssh jw service nginx restart
