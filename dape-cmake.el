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

;;; Code:

(require 'dape)
(require 'cl-lib)

;;;###autoload
(defun dape-cmake-debug (&optional buffer-file)
  "Use BUFFER-FILE (defaults to the current buffer) as the base and start debugging the executable created by CMake with lldb-dap."
  (interactive)
  ;; 1) Find the builded cmake executable file.
  (let* ((exe (cmake-find-built-executable buffer-file)))
    (unless exe
      (user-error "No built executable found. Please run M-x cmake-build-debug first."))
    ;; 2) Copy the default lldb-dap settings.
    ;;    dape-configs is a defcustom value inside dape.el.
    (let* ((base (copy-tree (alist-get 'lldb-dap dape-configs)))
           ;; 3) Ensure that `command-cwd` is set to a "string".
           ;; Normally, just before dape starts, `command-cwd` is evaluated and replaced with a string path.
           ;; But because the config is passed directly to dape, that replacement isn’t performed.
           ;; To avoid this, `funcall` is used to run and evaluate `dape-command-cwd`.
           (base (plist-put base
                            'command-cwd
                            (funcall (plist-get base 'command-cwd))))
           ;; 4) Modify the PLIST.
           (config (thread-first base
                                 (plist-put :name "CMake-LLDB")
                                 (plist-put :program exe)
                                 (plist-put :cwd (file-name-directory exe)))))
      (message "modify config : %S" config)
      ;; 5) run.
      (dape config))))

(provide 'dape-cmake)
;;; dape-cmake.el ends here
