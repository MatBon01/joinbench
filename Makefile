documents = interim.pdf

.PHONY: all clean cleanaux spell

all: $(documents)

$(documents): %.pdf: %.tex
	latexmk -pdf $<

cleanaux:
	latexmk -c $(documents)

clean:
	latexmk -C $(documents)

spell:
	hunspell -l -d en_GB -t *.tex

lint:
	chktex *.tex