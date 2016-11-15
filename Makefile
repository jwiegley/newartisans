SITENAME = newartisans
SITEPORT = 8081

HAKYLL = $(shell pwd)/result/bin/$(SITENAME)

all: build

build:
	nix-build '<nixpkgs>' -A haskPkgs.$(SITENAME)

site: build
	$(HAKYLL) rebuild

watch:
	$(HAKYLL) watch

deploy: site
	docker build -t $(SITENAME):latest .
	eval $(docker-machine env vps)
	docker stop $(SITENAME)
	docker rm $(SITENAME)
	docker run -d -p $(SITEPORT):80 --name $(SITENAME) $(SITENAME)
