Gratte Papier
=============

Scanned document management.

Gratte Papier takes scanned documents in batch, store them in a folder, tag and OCR them. You can then search for them.

This is a very early release.

Installation
------------
You need [Elasticsearch](http://www.elasticsearch.org/) and [Tesseract](https://code.google.com/p/tesseract-ocr/) installed.

```bash
> git clone git@github.com:ostapneko/gratte-papier.git
> cd gratte-papier
> cabal clean && cabal configure && cabal install
```

The executable will be in `dist/build/gratte-papier`

Usage
-----

Output from -h:
```text
Usage: gratte-papier [options] [args]

In query mode (default), it will output a list of files matching the string given as arguments

In add mode (with the -a flag), the files given in the stdin will be tagged with the command's arguments

Options:
  -V                --verbose               Verbose mode
  -s                --silent                Silent mode
  -h                --help                  Show help
  -e HOST           --es-host=HOST          Elastic search host and port, defaults to http://localhost:9200
  -a                --add-mode              Specify that Gratte is to be used in add mode.
                                            Filepaths are taken from stdin
  -p PREFIX         --prefix=PREFIX         Prefixes the files with the prefix argument. 
                                            Defaults to 'doc'
  -f OUTPUT FOLDER  --folder=OUTPUT FOLDER  The output folder. Defaults to ~/.gratte
  -d                --dry-run               Run in dry mode: no files are copied and
                                            the payloads that would have been sent to ES
                                            are displayed to stdout
  -o                --ocr                   Uses OCR to try extract the text from
                                            the documents and add it as searchable metadata.
                                            Requires tesseract to be installed.
```
