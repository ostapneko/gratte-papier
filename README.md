# Gratte Papier

Scanned document management.

Gratte Papier takes scanned documents in batch, store them in a folder, tag and OCR them. You can then search for them via the command line or a web application(gratte-server).

This is a very early release.

~~Documentation is coming soon !~~

# Installation

## Ansible
The prefered way of getting it to run is to use the Ansible [gratte-box provisioning code](https://github.com/ostapneko/gratte-box). It only works on Ubuntu 12.04 64 bits now, but you shouldn't use Gratte-Papier outside of a VM for the moment anyway...

## Manual
Gratte-papier requires
- GHC 7.6.3
- cabal > 1.18
- ImageMagick
- Tesseract OCR
- poppler-utils
- elasticsearch
- coffee script (for the UI)

After everything is installed, simply go to the gratte-papier repo and run
`make`

The executables will be in the `bin` directory of the project folder

# Usage

## General Options

Use `gratte-papier -h` to get all the common options, such as the log file, the Elasticsearch host/port, etc.

## Adding documents

You add document via the command line interface. The best documentation I can give is the one you get by running `gratte-papier add --help`.

## Searching for document

### Server

Use `gratte-papier serve`. You need to have Elasticsearch running. By default the server listen on port 3000. To change that, use `gratte-papier serve -p PORT`.
There is also a UI which is cumbersome at the moment to deploy, but I'll have the server serve it soon enough (hopefully).

### Command line
You can also query via the command line. For more detail, try `gratte-papier search --help`
