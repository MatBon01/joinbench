documents = interim.pdf

.PHONY: all clean cleanaux

all: $(documents)

$(documents): %.pdf: %.tex
	latexmk -pdf $<

cleanaux:
	latexmk -c $(documents)

clean:
	latexmk -C $(documents)