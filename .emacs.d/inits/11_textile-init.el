;; textile.el
;; textile-mode
(autoload 'textile-mode "textile-mode"
"Major mode for editing Textile files" t)
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))


;; pandoc だと textile のフォーマットに完全に対応していないので、 redcloth を使う。
;; ただし bin/redcloth に変更が必要
;; http://blog.basyura.org/entry/20120104/p1
;;
;;(quickrun-add-command "textile" '((:command . "pandoc")
;;                                  (:exec . "%c -s %s")
;;                                  (:outputter . browser)))

(quickrun-add-command "textile" '((:command . "redcloth")
                                  (:exec . "more \"%HOME%\\style.css\" && %c %s")
                                  (:outputter . browser)))

(add-to-list 'quickrun-file-alist '("\\.textile$" . "textile"))

