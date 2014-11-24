LP_NAME = literate-schemer
SCRBL_FILE = $(LP_NAME).scrbl
GENERATED_LP_FILES = $(LP_NAME).{pdf,html} scribble-common.js scribble-style.css scribble.css

pdf:
	scribble --pdf $(SCRBL_FILE)

html:
	scribble --html $(SCRBL_FILE)

clean:
	rm -f $(GENERATED_LP_FILES)
	rm -f *~
