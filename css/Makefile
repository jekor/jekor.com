sources := $(shell ls *.ccss)
csses := $(sources:.ccss=.css)
# clevercss := python ~/program/clevercss.py
clevercss := clevercss

all : $(csses)

%.css : %.ccss constants
	cat constants $< | $(clevercss) > $@

clean :
	rm $(csses)