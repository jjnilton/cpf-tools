;;; cpf-tools-test.el --- Tests for cpf-tools -*- lexical-binding: t; -*-

(require 'ert)
(require 'cpf-tools)

(ert-deftest test-validate-cpf ()
  "Test CPF validation logic."
  ;; Valid CPFs
  (should (validate-cpf "92484107900"))
  ;; Invalid CPFs
  (should-not (validate-cpf "00000000000"))
  (should-not (validate-cpf "12345678912")))

(ert-deftest test-validate-cnpj ()
  "Test CNPJ validation logic."
  ;; Valid CNPJs
  (should (validate-cnpj "43644726000170"))
  (should (validate-cnpj "43.644.726/0001-70"))
  ;; Invalid CNPJs
  (should-not (validate-cnpj "00000000000000")))

(ert-deftest test-format-cpf ()
  "Test CPF formatting."
  (should (string= (format-cpf "12345678910") "123.456.789-10"))
  (should-error (format-cpf "123")))

(ert-deftest test-format-cnpj ()
  "Test CNPJ formatting."
  (should (string= (format-cnpj "12345678000190") "12.345.678/0001-90"))
  (should-error (format-cnpj "123")))
