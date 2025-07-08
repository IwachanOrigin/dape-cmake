;;; cmake-exec.el --- ビルド済み実行ファイナリのパス取得 -*- lexical-binding: t; -*-

;; 依存:
;;   - cmake-build-directory      （cmake-build.el で定義）
;;   - cmake-build-config         （cmake-build.el で定義）

(require 'cl-lib)
(require 'cmake-build)

(defgroup cmake-exec nil
  "cmake で生成された実行ファイルの検索ユーティリティ。"
  :group 'tools)

(defcustom cmake-exec-target-name nil
  "メイン実行ファイル名（拡張子なし）。
nil なら検索ディレクトリ内で最初に見つかった実行ファイルを返す。"
  :type '(choice (const :tag "自動検出" nil) string)
  :group 'cmake-exec)

(defcustom cmake-build-config "Debug"
  ""
  :type 'string
  :group 'cmake-exec)

(defun cmake--exec-search-pattern ()
  "OS に応じた実行ファイル拡張子を返す正規表現。"
  (if (eq system-type 'windows-nt)
      "\\.exe\\'"   ; Windows は .exe
    ""))            ; Unix 系は拡張子なしも想定

;;;###autoload
(defun cmake-find-built-executable (&optional buffer-file)
  "BUFFER-FILE（省略時は現在バッファ）を基に
ビルド生成された実行ファイルのフルパスを取得する。見つからなければ nil。"
  (interactive)
  (let* ((root (cmake-utils-find-topmost-cmake-file-dir buffer-file)))
    (unless root
      (user-error "最上位 CMakeLists.txt が見つかりません"))
    ;; ------------------------------------------------------------
    ;; ★ 検索ディレクトリ候補を生成
    ;;   build/              （シングルコンフィグ）
    ;;   build/<CONFIG>/     （マルチコンフィグ）
    ;;   build/bin/          （CMAKE_RUNTIME_OUTPUT_DIRECTORY）
    ;;   build/bin/<CONFIG>/ （同上 + マルチコンフィグ）
    ;; ------------------------------------------------------------
    (let* ((build-dir (expand-file-name cmake-build-directory root))
           (bin-dir   (expand-file-name "bin" build-dir))
           (search-dirs
            (cl-remove-if-not
             #'file-directory-p
             (list (expand-file-name cmake-build-config bin-dir)
                   bin-dir
                   (expand-file-name cmake-build-config build-dir)
                   build-dir)))
           (pattern (cmake--exec-search-pattern))
           (target  nil))
      ;; ----------------------------------------------------------
      ;; 明示ターゲット名があれば優先
      ;; ----------------------------------------------------------
      (setq target
            (or
             ;; 明示ターゲット優先
             (when cmake-exec-target-name
               (cl-some (lambda (dir)
                          (let* ((exe (if (eq system-type 'windows-nt)
                                          (concat cmake-exec-target-name ".exe")
                                        cmake-exec-target-name))
                                 (path (expand-file-name exe dir)))
                            (when (file-exists-p path) path)))
                        search-dirs))
             ;; 自動検出
             (cl-some (lambda (dir)
                        (cl-some (lambda (f)
                                   (when (and (file-executable-p f)
                                              (not (file-directory-p f)))
                                     f))
                                 (directory-files dir t pattern)))
                      search-dirs)))
      ;; ----------------------------------------------------------
      ;; 返却 ＆ インタラクティブ表示
      ;; ----------------------------------------------------------
      (if (called-interactively-p 'interactive)
          (if target
              (message "%s" target)
            (message "実行ファイルが見つかりません: %s" search-dirs)))
      target)))

(provide 'cmake-exec)
;;; cmake-exec.el ends here
