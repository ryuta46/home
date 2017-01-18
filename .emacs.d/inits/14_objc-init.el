;; iOS SDKへのPATH
(defvar xcode:sdk "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk")

;; TABスペースを使う
(add-hook 'objc-mode-hook
          '(lambda()
             (setq c-basic-offset 4)
             (setq tab-width 4)
             (setq indent-tabs-mode t)))


;; .hファイルもobjc-modeで開くけるようにする(ただし @interface キーワードがある場合のみ)
(add-to-list 'magic-mode-alist
             `(,(lambda ()
                  (and (string= (file-name-extension buffer-file-name) "h")
                       (re-search-forward "@\\<interface\\>"
                                          magic-mode-regexp-match-limit t)))
               . objc-mode))

;; .m と .mm は objc-mode で開く
(add-to-list 'auto-mode-alist '("\\.m\\'" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

;; ロード
(require 'ac-company)
;; ac-company で company-xcode を有効にする
(ac-company-define-source ac-source-company-xcode company-xcode)
;; objc-mode で補完候補を設定
(setq ac-modes (append ac-modes '(objc-mode)))
;; hook
(add-hook 'objc-mode-hook
         (lambda ()
           (define-key objc-mode-map (kbd "\t") 'ac-complete)
           ;; XCode を利用した補完を有効にする
           (push 'ac-source-company-xcode ac-sources)
           ;; C++ のキーワード補完をする Objective-C++ を利用する人だけ設定してください
;;           (push 'ac-source-c++-keywords ac-sources)
         ))

