;; ロード
(require 'auto-complete)
(require 'auto-complete-config)
;; 対象の全てで補完を有効にする
(global-auto-complete-mode t)

;; 補完ウィンドウ内でのキー定義
(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)
(define-key ac-completing-map (kbd "M-/") 'ac-stop)


;; 補完を自動で起動するか. t = 自動起動. 数値 = 数値文字数入力自動起動. nil = 自動起動しない
(setq ac-auto-start 2)
;; 起動キーの設定. 自動起動しない場合に設定
;; (ac-set-trigger-key "TAB")
;; 候補の最大件数 デフォルトは 10件
(setq ac-candidate-max 20)


