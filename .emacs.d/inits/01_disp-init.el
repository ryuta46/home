;; color-theme
;; (load-theme 'manoj-dark t)
(load-theme 'tango-dark t)

;; 画面右端で折り返さない
(setq-default truncate-lines t)
(setq truncate-partial-width-windows t)

;; 行番号を表示
(global-linum-mode t)

;; 4 タブ
(setq default-tab-width 4)

;; スペース、タブ文字などを表示
(require 'whitespace)
(setq whitespace-style '(face           ; faceで可視化
                         trailing       ; 行末
                         tabs           ; タブ
                                        ;empty          ; 先頭/末尾の空行
                         spaces         ; スペース
                         space-mark     ; 表示のマッピング
                         tab-mark
                         ))

;; 全角スペースとタブ表示のマッピング
;; サンプルなどでは
;; □
;; が使われることが多いが、幅がずれてしまう問題が発生したため
;; ⬜
;; を使っている。（環境依存の文字らしいので、場合によっては使えないかもしれない)
(setq whitespace-display-mappings
;;;      '((space-mark ?\u3000 [?\⬜])
      '((space-mark ?\u3000 [?\・])
         ;; タブを表示するとずれるので、非表示
;;        (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])
       ))

;; 全角スペースのみ表示
(setq whitespace-space-regexp "\\(\x3000+\\)")

;; スペースの色
(set-face-foreground 'whitespace-space "DarkSlateGray")
(set-face-background 'whitespace-space nil)
(set-face-bold-p 'whitespace-space t)

;; タブの色
(set-face-foreground 'whitespace-tab "DarkRed")
(set-face-underline  'whitespace-tab t)
(set-face-background 'whitespace-tab nil)

;; スペースなどの表示有効
(global-whitespace-mode 1)

