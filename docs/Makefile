STYLES := \
	tufte-pandoc-css/tufte-css/tufte.css \
	tufte-pandoc-css/pandoc.css \
	tufte-pandoc-css/pandoc-solarized.css \
	tufte-pandoc-css/tufte-extra.css

all: index.html
	$(MAKE) -C ../stochaskell doc
	rm -rf doc
	cp -rv ../stochaskell/docs/stochaskell/ doc

%.html: %.md tufte-pandoc-css/tufte.html5 $(STYLES)
	pandoc \
		--katex \
		--section-divs \
		--from markdown+tex_math_single_backslash \
		--filter pwcite \
		--filter pandoc-citeproc \
		--filter pandoc-sidenote \
		--to html5+smart \
		--template=tufte-pandoc-css/tufte \
		$(foreach style,$(STYLES),--css $(style)) \
		--output $@ \
		$<
