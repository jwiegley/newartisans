all: build publish

build:
	nix build -f .

publish:
	rclone sync -v						\
	    ./result/share/html/newartisans			\
	    fastmail:/johnw.newartisans.com/files/newartisans

clean:
	rm -f result
