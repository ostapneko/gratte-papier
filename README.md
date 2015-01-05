# Gratte Papier

Scanned document management.

Gratte Papier takes scanned documents in batch, store them in a folder, tag and OCR them. You can then search for them via the command line or a minimal web front-end.

This is a very early release.

# Installation

## Manual
To make Gratte-papier, you require
- GHC 7.6.3
- cabal > 1.18
- m4
- coffee script (for the UI)

After everything is installed, simply go to the gratte-papier repo and run `make`

The executables will be in the `bin` directory of the project folder

# Usage

## Setup

To query, make sure you have Elasticsearch running

To add new documents, you also need
- ImageMagick
- Tesseract OCR
- poppler-utils
- elasticsearch

Also, you would need to have `/var/gratte` and `var/log/gratte` folders with read/write access, though they can be overriden by the appropritate options (see below).

## General Options

WARNING: use those options for Elasticsearch if you don't want to expose its content to the local network!
```
discovery.zen.ping.multicast.enabled: false 
node.local: true
```

Use `gratte-papier -h` to get all the common options, such as the log file, the Elasticsearch host/port, etc.

## Adding documents

You add document via the command line interface. The best documentation I can give is the one you get by running `gratte-papier add --help`.

## Reindexing

If you import files from somewhere else (or decrypted them, see below), you need to refresh Elasticsearch. Run `gratte-papier reindex`

## Searching for document

### Server

Use `gratte-papier serve`. You need to have Elasticsearch running. By default the server listen on port 3000. To change that, use `gratte-papier serve -p PORT`.
You can then access the app at `localhost:3000`

### Command line
You can also query via the command line. For more detail, try `gratte-papier search --help`

## Saving documents
In case you want to encrypt your Gratte folder file by file to (say) save it on Dropbox, you can use `gratte-papier encrypt -e /path/to/encrypted/folder`. You can decrypt using `gratte-papier decrypt -e /path/to/encrypted/folder`. In both cases, it will only synchronize the files that are not present in the target directory. The files are encrypted using CBC / AES 256.
Also, if you don't want to retype your password every time, you can write it to a file (remove the newline) and invoke the command like so: `gratte-papier --password-file /path/to/password-file [encrypt|decrypt] -e /path/to/encrypted/folder`
