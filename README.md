Tools for dealing with CPF and CNPJ numbers.

## Installation

```
<kbd>M</kbd><kbd>x</kbd> package-install-file cpf-tools.el
```

Or just place `cpf-tools.el` file in the `load-path` directory and `(require 'cpf-cnpj)`.

## Usage

To generate valid CPF and CNPJ:

- (cpf-tools/generate-cpf) => "12345678910"
- (cpf-tools/generate-cnpj) => "12345678000190"

Note: these interactive functions accepts prefixes:

- nil: outputs the unformatted string to the *Messages* buffer
- C-u: inserts the unformatted generated CPF/CNPJ string at point
- C-u C-u: outputs the generated and formatted CPF/CNPJ to the *Messages* buffer
- C-u C-u C-u: inserts and formats the generated CPF/CNPJ string at point

To format CPF and CNPJ:

- (cpf-tools/format-cpf-region (point-min) (point-max));  => "123.456.789-10"
- (cpf-tools/format-cnpj-region (point-min) (point-max)) => => "12.345.678/0001-90"
- (cpf-tools/format-cpf-cnpj-region (point-min) (point-max)) => "123.456.789-10" or "12.345.678/0001-90"

