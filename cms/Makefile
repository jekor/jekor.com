# jekor.com CMS CGI

cgi := cms.cgi

lhss := $(shell find . -name "*.lhs" -print)

all : $(cgi)

doc : cms.pdf

cms.cgi : $(lhss)
	ghc -Wall -fglasgow-exts -threaded --make -o $(cgi) $^

cms.pdf : CMS.tex
	pdflatex $<

CMS.tex : $(lhss)
	lhs2TeX CMS.lhs > $@

install : cms.cgi
	mv ~/.cabal/bin/cms.cgi ~/.cabal/bin/cms.cgi.backup
	cp cms.cgi ~/.cabal/bin/cms.cgi
	-killall $(cgi)

tags : $(lhss)
	hasktags -e $^

clean :
	find . -iregex ".*\.\(o\|hi\|o-boot\|hi-boot\)" -print0 | xargs -0 rm

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