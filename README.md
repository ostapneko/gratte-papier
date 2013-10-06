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
  -V                     --verbose                    Verbose mode
                         --silent                     Silent mode
  -h                     --help                       Show help
  -e HOST                --es-host=HOST               Elastic search host and port, defaults to http://localhost:9200
                         --es-index=INDEX             Elastic search index, defaults to 'gratte'
                         --folder=OUTPUT FOLDER       The output folder. Defaults to ~/.gratte
  -o                     --ocr                        Uses OCR to try extract the text from the documents and add it as searchable metadata. Requires tesseract to be installed.
                         --log--file=PATH             The log file. Defaults to /var/log/gratte/gratte.log
  -f c[ompact]|d[etail]  --format=c[ompact]|d[etail]  The output format in query mode. 'compact' will spit the file paths. 'detail' spits results in human-readable format. Defaults to 'detail'.
  -p i[mage]|t[text]     --pdf-mode=i[mage]|t[text]   The text recognition mode for PDF files, when used in conjonction with -o. '-p image' will consider the pdf as an image, while '-p text' will treat the PDF as text. This option is mandatory if you are scanning at least one PDF file with OCR.
  -n SIZE                --result-size=SIZE           The size of the result list. Defaults to 100.
  -t "TITLE"             --title="TITLE"              The title of the documents. If more than one documents are present, add a page number after it (e.g. "My doc (Page 1)", etc. )
  -s "NAME"              --sender="NAME"              The documents' sender
  -r "NAME"              --recipient="NAME"           The documents' recipient (who these documents where addressed to)
  -d "MONTH YEAR"        --date="MONTH YEAR"          The documents' month (optionaly) and year. If provided, the date MUST be in the form "September 2013".
  -T TAG1,TAG2           --tags=TAG1,TAG2             Add a comma or colon separated list of tags to the document. Only useful in add mode.
```
