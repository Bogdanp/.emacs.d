vendors=`find vendor -maxdepth 1 | perl -0pe 's/\n/ -L /g'`
elpa=`find elpa -maxdepth 1 | perl -0pe 's/\n/ -L /g'`

all: clean
	Emacs \
		-batch \
		-L ${vendors} . \
		-L ${elpa} . \
		-f batch-byte-compile ./elpa/**/*.el

clean:
	find . -name "*.elc" -delete
