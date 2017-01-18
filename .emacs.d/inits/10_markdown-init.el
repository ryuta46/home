;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
"Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;;(quickrun-add-command "markdown" '((:command . "pandoc")
;;                                  (:exec . "%c -s %s")
;;                                  (:outputter . browser)))

(add-to-list 'quickrun-file-alist '("\\.md$" . "markdown"))


(setq markdown-command "/usr/local/bin/multimarkdown")

