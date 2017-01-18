;; スペルチェッカ Aspell の設定
(add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
(setq-default ispell-program-name "aspell")
(eval-after-load "ispell"
 '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

