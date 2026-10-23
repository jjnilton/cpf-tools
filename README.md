# cpf-tools.el

Tools for working with CPF and CNPJ numbers.

### Features
- Generate CPF/CNPJ (optionally formatted);
- Format region as CPF/CNPJ;
- Insert generated CPF/CNPJ;
- Validate generated CPF/CNPJ;
- Fill missing numbers of incomplete CPF/CNPJ.

![Screen cast](screencast.gif)

## Installation

### Using `package-install-file`

<kbd>M</kbd><kbd>x</kbd> `package-install-file cpf-tools.el`

### Load path

Or just place `cpf-tools.el` file in the `load-path` directory and `(require 'cpf-tools)`.

### With use-package

```elisp
(use-package cpf-tools
  :ensure t
  :vc (:url "https://github.com/jjnilton/cpf-tools" :rev :newest))
```

## Usage

To generate valid CPF and CNPJ:

- `(cpf-tools/generate-cpf)` => "12345678910"
- `(cpf-tools/generate-cnpj)` => "12345678000190"

Note: these functions are interactive and can be used the following prefixes:

- `nil`: outputs the unformatted string to the `*Messages*` buffer;
- <kbd>C</kbd><kbd>u</kbd>: inserts the unformatted generated CPF/CNPJ string at point;
- <kbd>C</kbd><kbd>u</kbd> <kbd>C</kbd><kbd>u</kbd>: outputs the generated and formatted CPF/CNPJ to the `*Messages*` buffer;
- <kbd>C</kbd><kbd>u</kbd> <kbd>C</kbd><kbd>u</kbd> <kbd>C</kbd><kbd>u</kbd>: inserts and formats the generated CPF/CNPJ string at point.

To find valid CPFs and CNPJs matching incomplete patterns with wildcards (`*` or `?`):

- `(cpf-tools/complete-cpf "***.781.235-**")` (interactive command `M-x cpf-tools/complete-cpf`)
- `(cpf-tools/complete-cnpj "12.345.678/****-**")` (interactive command `M-x cpf-tools/complete-cnpj`)

- `(cpf-tools/format-cpf-region (point-min) (point-max))`  => "123.456.789-10"
- `(cpf-tools/format-cnpj-region (point-min) (point-max))` => "12.345.678/0001-90"
- `(cpf-tools/format-cpf-cnpj-region (point-min) (point-max))` => "123.456.789-10" or "12.345.678/0001-90"

## Info Manual

To compile the Texinfo documentation source (`cpf-tools.texi`) into an Emacs `.info`:

```bash
makeinfo cpf-tools.texi # requires texinfo
```

This generates `cpf-tools.info`, which can be viewed directly within Emacs via `C-h i` (Info manual browser).

## Testing

To run unit tests with the Emacs' built-in `ert` framework:

```bash
emacs --batch -Q -L . -l cpf-tools.el -l test/cpf-tools-test.el -f ert-run-tests-batch-and-exit

# - `--batch`: Runs Emacs in non-interactive (headless) batch mode.
# - `-Q`: Quick startup; suppresses loading initialization files and site-start configuration # logs.
# - `-L .`: Adds the current directory (`.`) to Emacs's `load-path`.
# - `-l cpf-tools.el`: Loads the package file.
# - `-l test/cpf-tools-test.el`: Loads the test suite file.
# - `-f ert-run-tests-batch-and-exit`: Executes all tests and exits Emacs with status `0` on success or `1` on failure.
```

