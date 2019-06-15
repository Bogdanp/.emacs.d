vendors=`find vendor -maxdepth 1 | perl -0pe 's/\n/ -L /g'`
elpa=`find elpa -maxdepth 1 | perl -0pe 's/\n/ -L /g'`

all: clean
	/Applications/Emacs.app/Contents/MacOS/Emacs \
		-batch \
		-L ${vendors} . \
		-L ${elpa} . \
		-f batch-byte-compile ./elpa/**/*.el ./vendor/**/*.el

clean:
	find . -name "*.elc" -delete
