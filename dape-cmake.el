;;; dape-cmake.el --- Debug an executable generated with CMake using dape + lldb-dap. -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Yuji Iwanaga

;; Author: Yuji Iwanaga <nm7.ty.nt.abc@gmail.com>
;; URL: https://github.com/IwachanOrigin/dape-cmake
;; Version: 1.0.0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; How to Use
;; Running M-x dape-cmake-debug starts the debugging session.
;; Other operations are the same as in dape.
;; When performing a debug run, it is recommended to use it in combination with Repeat-mode.

;; To set arguments, run M-x dape-cmake-set-args and configure them via the prompt.
;; A .dape-args file will be created in the project root (i.e., the directory containing the top-level CMakeLists.txt).
;; If a .dape-args file already exists, it will be parsed and the arguments will be loaded automatically.

;;; Code:

(require 'dape)
(require 'json)
(require 'subr-x)
(require 'cl-lib)
(require 'cmake-utils)

(defgroup dape-cmake nil
  "dape + cmake debug helper."
  :group 'tools)

(defcustom dape-cmake-args-file ".dape-args"
  "The name of the file that saves debug arguments, placed in the project root."
  :type 'string
  :group 'dape-cmake)

(defun dape-cmake--project-root (&optional dir)
  "Return the root of the CMake project containing DIR (defaults to `default-directory`)."
  (cmake-utils-find-topmost-cmake-file-dir (or dir default-directory)))

(defun dape-cmake--args-path (&optional dir)
  "Return the absolute path to .dape-args, using DIR as the base."
  (expand-file-name dape-cmake-args-file
                    (or (dape-cmake--project-root dir)
                        (user-error "Top level CMakeLists.txt not found."))))

(defun dape-cmake--parse-args-buffer ()
  "Internal function that inspects the current buffer content and returns the argument list."
  (goto-char (point-min))
  (let ((first (char-after)))
    (cond
     ;; Json array
     ((eq first ?\[) (json-read))
     ;; Elisp sexp
     ((eq first ?\() (read (current-buffer)))
     ;; Space-separated text
     (t (split-string-and-unquote (string-trim (buffer-string)))))))

(defun dape-cmake--load-args (&optional dir)
  "Read the argument file and return it as a Vector. If it doesn't exist, return nil."
  (let ((path (dape-cmake--args-path dir)))
    (when (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (vconcat (dape-cmake--parse-args-buffer))))))

(defun dape-cmake-set-args (&optional dir)
  "Configure and save the debug arguments for the current project. A blank line or empty input deletes the existing settings."
  (interactive)
  (let* ((current (dape-cmake--load-args dir))
         (default-str (when current
                        (mapconcat #'identity current " ")))
         (prompt (if default-str
                     (format "Program args [%s] (SPC separated, RET = clear): " default-str)
                   "Program args (SPC separated, RET = clear): "))
         (input (read-from-minibuffer prompt nil nil nil 'dape-args-history))
         (path (dape-cmake--args-path dir)))
    (if (string-empty-p input)
        ;; clear
        (when (file-exists-p path)
          (delete-file path)
          (message "Removed .dape-args file: %s" path))
      (let ((args (split-string-and-unquote input)))
        (with-temp-file path
          (insert (json-encode args)))
        (message "Saved .dape-args file: %s" args)))))

;;;###autoload
(defun dape-cmake-debug (&optional buffer-file)
  "Use BUFFER-FILE (defaults to the current buffer) as the base and start debugging the executable created by CMake with lldb-dap.
Automatically load arguments from `.dape-args` in the project root (no arguments if the file is absent)."
  (interactive)
  ;; 1) Find the builded cmake executable file.
  (let* ((exe (cmake-find-built-executable buffer-file)))
    (unless exe
      (user-error "No built executable found. Please run M-x cmake-build-debug first."))
    ;; 2) Load arguments from `.dape-args`. No arguments if the file is absent.
    (let* ((args (or (dape-cmake--load-args buffer-file) []))
           ;; 3) Copy the default lldb-dap settings.
           ;;    dape-configs is a defcustom value inside dape.el.
           (base (copy-tree (alist-get 'lldb-dap dape-configs)))
           ;; 4) Ensure that `command-cwd` is set to a "string".
           ;; Normally, just before dape starts, `command-cwd` is evaluated and replaced with a string path.
           ;; But because the config is passed directly to dape, that replacement isn’t performed.
           ;; To avoid this, `funcall` is used to run and evaluate `dape-command-cwd`.
           (base (let ((val (plist-get base 'command-cwd)))
                   (if (functionp val)
                       (plist-put base 'command-cwd (funcall val))
                     base)))
           ;; 5) Modify the PLIST.
           (config (thread-first base
                                 (plist-put :name "CMake-LLDB")
                                 (plist-put :program exe)
                                 (plist-put :cwd (file-name-directory exe))
                                 (plist-put :args args))))
      (message "modify config : %S" config)
      ;; 6) run.
      (dape config))))

(provide 'dape-cmake)
;;; dape-cmake.el ends here
