documents = interim.pdf final.pdf

.PHONY: all clean cleanaux spell

all: $(documents)

$(documents): %.pdf: %.tex
	latexmk -pdf $<

cleanaux:
	latexmk -c $(documents)

clean:
	latexmk -C $(documents)

spell:
	hunspell -d en_GB -p ./.hunspell -t *.tex */*.tex */*/*.tex

spellcheck:
	hunspell -l -d en_GB -p ./.hunspell -t *.tex */*.tex */*/*.tex

lintcheck:
	-chktex -q -v0 *.tex

lint:
	-chktex *.tex
