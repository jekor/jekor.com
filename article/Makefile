# jekor.com articles

sources := $(shell ls *.muse)
articles := $(sources:.muse=.html)

all : $(articles)

%.html : %.muse publish
	emacs -q -batch -l muse-init.el -f muse-batch-publish-files xhtml $<
	./publish `basename $< .muse`

clean :
	rm $(articles)
