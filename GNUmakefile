

targets = \
	sec1-1-s5-gl.html \
	sec1-2-s5-gl.html \
	sec1-3-s5-gl.html \
	sec1-4-s5-gl.html \
	sec1-5-s5-gl.html \
	sec2-1-s5-gl.html \
	sec2-2-s5-gl.html


md_format = \
	markdown+pandoc_title_block+pipe_tables+table_captions+escaped_line_breaks+implicit_figures+strikeout+tex_math_dollars+latex_macros+fenced_code_blocks

math_opt = --latexmathml
#math_opt = --jsmath
#math_opt = --mathjax

slide_opts = \
	--standalone --self-contained --slide-level=2 \
	$(math_opt)
##	--incremental

glad_imaged = images

$(glad_imaged)/%-s5-gl %-gl.html: %-gl.htex
	mkdir -p $(glad_imaged)
	$(RM) -fr $(glad_imaged)/$(shell basename $@ .html)
	gladtex -r 160 -d $(glad_imaged)/$(shell basename $@ .html) $<

%-s5.html: %.txt
	pandoc -f $(md_format) -t s5 $(slide_opts) -o $@ $<

%-s5-gl.htex: %.txt
	pandoc -f $(md_format) -t s5 $(slide_opts) --gladtex  -o $@ $<

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
	$(RM) -r $(glad_imaged)
##	$(RM) *.dvi *.pdf
##	$(RM) *.aux *.log *.nav *.out *.snm *.toc *.vrb
##	$(RM) codeCount.table
