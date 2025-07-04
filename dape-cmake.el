;;; dape-cmake.el --- CMake で出来た実行ファイルを dape + lldb-dap ですぐデバッグ -*- lexical-binding: t; -*-

;; ❶ 事前条件 ─────────────────────────────────────────────────────────
;;   * myfind-topmost-cmake-dir / cmake-build-directory / cmake-build-config
;;     cmake-find-built-executable がすでに動く
;;   * LLVM/bin が PATH にあり lldb-vscode か lldb-dap が呼び出せる
;;   * MELPA などからパッケージ dape をインストールずみ
;;
;; ❷ 目的 ───────────────────────────────────────────────────────────
;;   - M-x dape-cmake-debug で
;;       ① ビルド生成バイナリを自動検出
;;       ② lldb-dap を選択済みの dape セッションを即起動
;;   - 手動で毎回 :program や :cwd を入力する手間を無くす
;;
;; ❸ 使い方 ─────────────────────────────────────────────────────────
;;   (require 'dape-cmake)           ; init.el などに追記
;;   C/C++ バッファで
;;     M-x cmake-build        （←必要なら）
;;     M-x dape-cmake-debug   （←デバッグ開始）
;;
;;   通常の dape キーバインド例:
;;     b : ブレークポイント、c : 続行、n : step-over、i : step-into、q : 終了
;;
;; ❹ 参考: https://github.com/svaante/dape
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dape)           ; dape 本体
(require 'cl-lib)

;;------------------------------------------------------------
;; 1) lldb アダプタのパスを任意で上書きしたい場合
;;    dape は既定で 'lldb' アダプタを組み込んでいるため、
;;    PATH に lldb-vscode があればこの節は不要です。
;;
;; (add-to-list
;;  'dape-adapters
;;  '(lldb-custom
;;    :command "/opt/llvm/bin/lldb-vscode"  ; 独自パス
;;    :temp    yes))                        ; Windows なら拡張子 .exe を付与
;;------------------------------------------------------------

(defcustom dape-lldb-command "lldb-dap"
  "PATH から取得する lldb debug adapter 実行ファイル名
Windows の場合は \"lldb-dap.exe\" などに合わせる"
  :type 'string  :group 'dape)

(defun dape--cmake-build-file (&optional buffer-file)
  "現在 (または BUFFER-FILE) を基準に、ビルド済み実行ファイルを返す。
見つからなければエラーを投げる。"
  (or (cmake-find-built-executable buffer-file)
      (user-error "ビルド済み実行ファイルが見つかりません。先に M-x cmake-build してください。")))

;;;###autoload
;;(defun dape-cmake-debug (&optional buffer-file)
;;  "cmake で生成した実行ファイルを lldb-dap でデバッグ。"
;;  (interactive)
;;  (let* ((exe (dape--cmake-build-file buffer-file))
;;         (config `(:name    "CMake-LLDB"
;;                            :type    "lldb"
;;                            :request "launch"
;;                            :command dape-lldb-command
;;                            :program exe
;;                            :cwd     (file-name-directory exe)
;;                            :stopOnEntry :json-false)))
;;    ;; dape は (dape CONFIG) の形で直接起動可能
;;    (dape config)))

(defun dape-cmake-debug (&optional buffer-file)
  "BUFFER-FILE (省略時は現在バッファ) を基点に
CMake が作った実行ファイルを lldb-dap でデバッグ起動する。"
  (interactive)
  (let* ((exe (cmake-find-built-executable buffer-file)))
    (unless exe
      (user-error "ビルド済み実行ファイルが見つかりません。まず M-x cmake-build"))
    ;; ① 既定の lldb-dap 設定をコピー
    (let* ((base   (copy-tree (alist-get 'lldb-dap dape-configs))) ;← built-in
           ;; ② PLIST を書き換え
           (config (thread-first base
                     (plist-put :name "CMake-LLDB")
                     (plist-put :program exe)
                     (plist-put :cwd (file-name-directory exe)))))
      (dape config))))                 ; ③ 起動！

                                        ;(with-eval-after-load 'cc-mode
                                        ;  ;; C-c d でワンキー起動（任意）
                                        ;  (define-key c-mode-base-map (kbd "C-c d") #'dape-cmake-debug))

(provide 'dape-cmake)
;;; dape-cmake.el ends here
