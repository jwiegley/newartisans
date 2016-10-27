HAKYLL = $(shell pwd)/result/bin/newartisans

all: $(HAKYLL)
	echo NewArtisans.com is built

$(HAKYLL):
	nix-build '<nixpkgs-next>' --fallback --show-trace -A haskell801Packages.newartisans

site:
	$(HAKYLL) rebuild

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
