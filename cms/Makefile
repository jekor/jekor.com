# jekor.com CMS CGI

cgi := cms.cgi
lhss := $(shell find . -name "*.lhs" -print)
binary := dist/build/$(cgi)/$(cgi)

all : $(binary)

doc : cms.pdf

configure : $(lhss)
	cabal configure

$(binary) : $(lhss)
	TPG_DB="cms" TPG_USER="cms" cabal build

install : $(binary)
	mv ../$(cgi) ../$(cgi).old
	cp $(binary) ../$(cgi)
	strip ../$(cgi)

cms.pdf : CMS.tex
	pdflatex $<

CMS.tex : $(lhss)
	lhs2TeX CMS.lhs > $@

tags : $(lhss)
	hasktags -e $^

lint : $(lhss)
	hlint $^

clean :
	cabal clean

metrics : $(lhss)
	@echo -e -n "\tSLOC: "
	@lhs2TeX --code CMS.lhs | wc --lines | tr -d "\n"
	@echo -e -n "\tgzipped bytes: "
	@lhs2TeX --code CMS.lhs | gzip | wc --bytes | tr -d "\n"
	@echo -e -n "\tsource chars: "
	@lhs2TeX --code CMS.lhs | wc --chars | tr -d "\n"
	@echo -e -n "\t  total chars: "
	@cat $^ | wc --chars | tr -d "\n"
	@echo
	@echo
	@ls -l $^