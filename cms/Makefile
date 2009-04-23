# Vocabulink CGI

fcgi := cms.fcgi

lhss := $(shell find . -name "*.lhs" -print)

all : $(fcgi)

doc : cms.pdf

cms.fcgi : $(lhss)
	ghc -Wall -fglasgow-exts -threaded -package fastcgi --make -o $(fcgi) $^

cms.pdf : CMS.tex
	pdflatex $<

CMS.tex : $(lhss)
	lhs2TeX CMS.lhs > $@

install : cms.fcgi
	install -o root -g root $(fcgi) /usr/local/bin
	-killall $(fcgi)
	spawn-fcgi -f "/usr/local/bin/$(fcgi) 2>> /tmp/cms.error.log" -p 10034 -u lighttpd -g lighttpd

tags : $(lhss)
	hasktags -e $^

clean :
	find . -iregex ".*\.\(o\|hi\|o-boot\|hi-boot\)" -print0 | xargs -0 rm

test :
	ghci -fglasgow-exts CMS.lhs

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