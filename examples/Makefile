.PHONY: all

all: atemporal-comparison.pdf atemporal-compact.pdf temporal.pdf min-dist.pdf

atemporal-compact.pdf:
	cd atemporal && make $@ && cp $@ ../
atemporal-comparison.pdf:
	cd atemporal && make $@ && cp $@ ../
temporal.pdf:
	cd temporal  && make $@ && cp $@ ../
min-dist.pdf:
	cd min-dist && make min-dist.pdf && cp min-dist.pdf ../

clean:
	rm -f min-dist.pdf atemporal-compact.pdf atemporal-comparison.pdf temporal.pdf
