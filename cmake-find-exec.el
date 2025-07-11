;;; cmake-find-exec.el --- Obtaining the path to the pre-built executable -*- lexical-binding: t; -*-

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
;; Called cmake-find-built-executable,
;; the following processing is performed starting from the file in the current buffer.
;;   1) Get the top level CMakeLists.txt directory path.
;;   2) Generate four directory paths, check for their existence, and remove any that are not found from the list.
;;   3) If `cmake-exec-target-name` is set, search using that target name first.
;;      If it is not set, run the auto-detection feature to locate the executable.
;;   4) Return the obtained executable file path. If called from interactive mode, display the executable file path first.

;;; Code:

(require 'cl-lib)
(require 'cmake-build)

(defgroup cmake-exec nil
  "A utility to search for executables generated by cmake."
  :group 'tools)

(defcustom cmake-exec-target-name nil
  "Name of the main executable file (without extension).
If nil, returns the first executable found in the search directory."
  :type '(choice (const :tag "auto-detection" nil) string)
  :group 'cmake-find-exec)

(defcustom cmake-build-config "Debug"
  "The string corresponding to the <CONFIG> section."
  :type 'string
  :group 'cmake-find-exec)

(defun cmake--exec-search-pattern ()
  "A regular expression that returns the executable file extension according to the operating system."
  (if (eq system-type 'windows-nt)
      "\\.exe\\'"   ; Windows is .exe
    ""))            ; Unix-like systems are assumed to have no extensions.

;;;###autoload
(defun cmake-find-built-executable (&optional buffer-file)
  "Get the full path to the build-generated executable based on BUFFER-FILE (or current buffer if omitted).
 If not found, nil."
  (interactive)
  (let* ((root (cmake-utils-find-topmost-cmake-file-dir buffer-file)))
    (unless root
      (user-error "Top level CMakeLists.txt not found."))
    ;; ------------------------------------------------------------
    ;; ★ Generate search directory candidates
    ;;   build/              （Singleconfig）
    ;;   build/<CONFIG>/     （Multiconfig）
    ;;   build/bin/          （CMAKE_RUNTIME_OUTPUT_DIRECTORY）
    ;;   build/bin/<CONFIG>/ （Same as above + multiconfig）
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
      ;; Preferred if explicit target name
      ;; ----------------------------------------------------------
      (setq target
            (or
             ;; Preferred if explicit target name
             (when cmake-exec-target-name
               (cl-some (lambda (dir)
                          (let* ((exe (if (eq system-type 'windows-nt)
                                          (concat cmake-exec-target-name ".exe")
                                        cmake-exec-target-name))
                                 (path (expand-file-name exe dir)))
                            (when (file-exists-p path) path)))
                        search-dirs))
             ;; Auto-detection
             (cl-some (lambda (dir)
                        (cl-some (lambda (f)
                                   (when (and (file-executable-p f)
                                              (not (file-directory-p f)))
                                     f))
                                 (directory-files dir t pattern)))
                      search-dirs)))
      ;; ----------------------------------------------------------
      ;; Return & Interactive Display
      ;; ----------------------------------------------------------
      (if (called-interactively-p 'interactive)
          (if target
              (message "%s" target)
            (message "Executable file not found: %s" search-dirs)))
      target)))

(provide 'cmake-find-exec)
;;; cmake-exec.el ends here
