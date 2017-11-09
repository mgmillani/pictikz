all: README.md pictikz.1

README.md: README.header README.body
	cat $^ > $@

pictikz.1: man.header man.body
	cat $^ > $@

man.body: README.body
	pandoc --to man --from markdown $^ > $@
