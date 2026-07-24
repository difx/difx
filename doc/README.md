# DiFX Documentation

This directory contains the DiFX documentation in Sphinx format.

## Building the Documentation

### Setup Virtual Environment

```bash
$ python -m venv ~/.venv
$ source ~/.venv/bin/activate
```

### Install Dependencies

```bash
$ pip install -r requirements.txt
```

### Build the Documentation

The documentation will be built in the `_build` directory.

```bash
$ make html # generate html files
$ make latexpdf # generate PDF files
```
