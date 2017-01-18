;; grepの設定
(require 'grep)
(define-key global-map (kbd "C-x g") 'grep)

;; grep のオプション
;; -i
;; -i 大文字小文字を区別せずにマッチングをする。
;; -n 検索結果の出力に行番号をつける。
;; -H 検索結果の出力にファイル名を付ける。 検索対象のファイルが1つの場合にはデフォルトではファイル名は出力されず、 複数の場合にはデフォルトでファイル名が出力される。 このオプションを付けるとたとえファイルが1つでもファイル名が出力される。
;; -r ディレクトリを再帰的にたどって検索する。
;; --exclude-dir 
;; -e OR
(setq grep-command-before-query "grep -i -nH -r --exclude-dir .svn -e ")

(defun grep-default-command ()
  (if current-prefix-arg
      (let ((grep-command-before-target
             (concat grep-command-before-query
                     (shell-quote-argument (grep-tag-default)))))
        (cons (if buffer-file-name
                  (concat grep-command-before-target
                          " *."
                          (file-name-extension buffer-file-name))
                (concat grep-command-before-target " ."))
              (+ (length grep-command-before-target) 1)))
    (car grep-command)))
(setq grep-command (cons (concat grep-command-before-query " .")
                         (+ (length grep-command-before-query) 1)))


