(load "~/venvs/my/schemalint/misc/flycheck-yaml-schemalint")
(require 'flycheck-yaml-schemalint)

(my:flycheck-executable-find-function-register
 "schemalint"
 (lambda ()
   (or (pickup:pickup-file "bin/schemalint") "schemalint")))

(add-hook 'yaml-mode-hook 'flycheck-mode)

;; (setq s "status:ERROR\terrortype:ParseError\tfilename:src/ng.yaml\tstart:5@0\tend:5@-1\t\tmsg:could not find expected ':' (while scanning a simple key)  where:['src/ng.yaml:5']\n")
;; (flycheck-yaml-schemalint:parse s nil nil)
;; (flycheck-yaml-schemalint:parse-ltsv-line s)
