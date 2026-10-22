;;; cpf-tools-test.el --- Tests for cpf-tools -*- lexical-binding: t; -*-

(require 'ert)
(require 'cpf-tools)

(ert-deftest test-validate-cpf ()
  "Test CPF validation logic."
  ;; Valid CPFs
  (should (cpf-tools-validate-cpf "92484107900"))
  ;; Invalid CPFs
  (should-not (cpf-tools-validate-cpf "00000000000"))
  (should-not (cpf-tools-validate-cpf "12345678912")))

(ert-deftest test-validate-cnpj ()
  "Test CNPJ validation logic."
  ;; Valid CNPJs
  (should (cpf-tools-validate-cnpj "43644726000170"))
  (should (cpf-tools-validate-cnpj "43.644.726/0001-70"))
  ;; Invalid CNPJs
  (should-not (cpf-tools-validate-cnpj "00000000000000")))

(ert-deftest test-format-cpf ()
  "Test CPF formatting."
  (should (string= (cpf-tools-format-cpf "12345678910") "123.456.789-10"))
  (should-error (cpf-tools-format-cpf "123")))

(ert-deftest test-format-cnpj ()
  "Test CNPJ formatting."
  (should (string= (cpf-tools-format-cnpj "12345678000190") "12.345.678/0001-90"))
  (should-error (cpf-tools-format-cnpj "123")))
