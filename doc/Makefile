BUILD = build
BOOKNAME = Exasol6vs7_DatavaultBuilder
TITLE = title.md
CHAPTERS = chapters/00-intro.md chapters/01-samplestack.md \
           chapters/02-results.md chapters/03-discussion.md \
		   chapters/04-conclusions.md chapters/05-appendix.md

all: book

book: pdf html #epub

clean:
	rm -r $(BUILD)

pdf: $(BUILD)/pdf/$(BOOKNAME).pdf

html: $(BUILD)/html/$(BOOKNAME).html

$(BUILD)/pdf/$(BOOKNAME).pdf: $(TITLE) $(CHAPTERS)
	mkdir -p $(BUILD)/pdf
	docker run --rm --volume `pwd`:/data pandoc/latex:2.6 -f markdown-implicit_figures -V geometry:margin=1.5in -o /data/$@ $^

$(BUILD)/html/$(BOOKNAME).html: $(TITLE) $(CHAPTERS)
	mkdir -p $(BUILD)/html
	docker run --rm --volume `pwd`:/data $(TOC) pandoc/latex:2.6 --standalone --to=html5 -o /data/$@ $^

.PHONY: all book clean pdf html #epub
