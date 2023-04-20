SITE = newartisans

all: generate build publish

generate:
	make -C $(HOME)/doc/org $(SITE)

build:
	nix build -f .

publish:
	rclone sync -v					\
	    ./result/share/html/$(SITE)			\
	    fastmail:/johnw.$(SITE).com/files/$(SITE)

clean:
	rm -f result
