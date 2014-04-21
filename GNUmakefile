

targets = \
	sec1-1-s5.html

md_format = \
	markdown+pandoc_title_block+pipe_tables+table_captions+escaped_line_breaks+implicit_figures+strikeout+tex_math_dollars

slide_opts = \
	--standalone --self-contained --slide-level=2 \
	--incremental


%-s5.html: %.txt
	pandoc -f $(md_format) -t s5 $(slide_opts) -o $@ $<

%-slidy.html: %.txt
	pandoc -f $(md_format) -t slidy $(slide_opts) -o $@ $<

%-slideous.html: %.txt
	pandoc -f $(md_format) -t slideous $(slide_opts) -o $@ $<

%-dzslides.html: %.txt
	pandoc -f $(md_format) -t dzslides $(slide_opts) -o $@ $<

%.html: %.txt
	pandoc -f $(md_format) -t html -s -o $@ $<

%.tex: %.txt
	pandoc -f $(md_format) -t beamer -s --slide-level=2 -o $@ $<


%.dvi %.log %.aux: %.tex
	platex $<


%.pdf: %.dvi
	dvipdfmx $(@:.pdf=.dvi)


all: $(targets)

codeCount.table.in:
	./codeCount.table.sh > $@

clean:
	$(RM) $(targets)
##	$(RM) *.dvi *.pdf
##	$(RM) *.aux *.log *.nav *.out *.snm *.toc *.vrb
##	$(RM) codeCount.table
