SHELL := /bin/bash
sasses := $(shell find www -name "text.x-sass")
markdowns := $(shell find www/article -name "text.x-web-markdown")
sync_options := -avz bin etc var www --exclude comment/* --exclude comments/* --exclude var/* --exclude graph/* jekor.com:jekor.com/

all : www/text.html www/articles www/articles/application.json $(sasses:x-sass=css) $(markdowns:x-web-markdown=html) $(markdowns:text.x-web-markdown=application.json) $(markdowns:text.x-web-markdown=comment/POST) $(markdowns:text.x-web-markdown=comments) $(markdowns:text.x-web-markdown=comments/application.json) www/articles/feed/application.rss+xml var/emails www/resume/text.html www/script/domcharts/bar/application.json www/jsonwrench/text.html www/jsonwrench/comment/POST

sync :
	rsync $(sync_options)

sync-test :
	rsync --dry-run $(sync_options)

%text.css : %text.x-sass
	sass $< > $@

var/sites.json : etc/sites
	map "jw string | jw name url" < $< | jw array > $@

www/text.html : www/articles/application.json var/sites.json template/front.html template/article-item.html template/site-item.html etc/analytics.js
	cat \
	<(jw name articles < www/articles/application.json) \
	<(jw name sites < var/sites.json) \
	<(jw string < etc/analytics.js | jw name analytics) \
	| jw merge | jigplate template/front.html template/article-item.html template/site-item.html > $@

var/nav.html : www/articles/application.json template/nav.html template/article-item.html template/site-item.html
	cat \
	<(jw name articles < www/articles/application.json) \
	<(jw name sites < var/sites.json) \
	| jw merge | jigplate template/nav.html template/article-item.html template/site-item.html > $@

www/article/%/application.json : www/article/%/text.x-web-markdown
	cat \
	<(cat <(echo -e "title\nauthor\ndate") <(head -n 3 $< | cut -d' ' -f2-) | jw ziplines) \
	<(echo $$(basename $$(dirname $<)) | jw string | jw name name) \
	<(pandoc --smart --section-divs --mathjax -t html5 < $< | jw string | jw name body) \
	| jw merge > $@

www/article/%/text.html : www/article/%/application.json www/articles/application.json template/article.html template/article-item.html template/nav.html etc/analytics.js var/nav.html
	cat $< \
	<(jw string < etc/analytics.js | jw name analytics) \
	<(jw string < var/nav.html | jw name nav) \
	| jw merge | jigplate template/article.html > $@

www/articles :
	mkdir $@

www/articles/application.json : $(markdowns:text.x-web-markdown=application.json)
	cat $^ | paste <(cat $^ | map jw lookup date) - | sort -r | cut -f2 | jw array > $@

www/articles/feed :
	mkdir $@

www/articles/feed/application.rss+xml : www/articles/application.json template/rss-item.xml template/rss.xml www/articles/feed
	cat \
	<(jw name items < $<) \
	<(date -R | tr -d "\n" | jw string | jw name date) \
	| jw merge | jigplate template/rss-item.xml template/rss.xml > $@

www/article/%/comment/POST :
	mkdir -p $$(dirname $@) && chmod 777 $$(dirname $@)
	ln -sf ../../../../bin/post-comment $@

www/article/%/comments :
	mkdir $@

www/article/%/comments/application.json :
	echo "[]" > $@ && chmod 666 $@

var :
	mkdir $@

var/emails : var
	echo "{}" > $@ && chmod 666 $@

www/resume/text.html : www/resume/text.x-web-markdown template/resume.html etc/analytics.js
	cat \
	<(head -n 1 $< | cut -d' ' -f2- | jw string | jw name title) \
	<(pandoc --from=markdown --to=html5 --smart --email-obfuscation=none $< | jw string | jw name body) \
	<(jw string < etc/analytics.js | jw name analytics) \
	| jw merge | jigplate template/resume.html > $@

www/script/domcharts/bar/application.json :
	mkdir -p www/script/domcharts/bar
	wget https://raw.github.com/jekor/domcharts/master/bar.js -O $@
