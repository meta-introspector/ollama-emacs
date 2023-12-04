;;; ollama.el --- ollama client for Emacs
(setq url-debug t)
;; Copyright (C) 2023 ZHOU Feng

;; Author: ZHOU Feng <zf.pascal@gmail.com>
;; URL: http://github.com/zweifisch/ollama
;; Keywords: ollama llama2
;; Version: 0.0.1
;; Created: 6th Aug 2023

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; ollama client for Emacs
;;

;;; Code:
(require 'json)
(require 'cl-lib)
(require 'url)

(defgroup ollama nil
  "Ollama client for Emacs."
  :group 'ollama)

(defcustom ollama:endpoint "http://localhost:11434/api/generate"
  "Ollama http service endpoint."
  :group 'ollama
  :type 'string)

(defcustom ollama:model "mistral"
  "Ollama model."
  :group 'ollama
  :type 'string)

(defcustom ollama:language "Chinese"
  "Language to translate to."
  :group 'ollama
  :type 'string)

(defun ollama-fetch (url prompt model)
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data
          (encode-coding-string
           (json-encode `((model . ,model) (prompt . ,prompt)))
           'utf-8)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      (decode-coding-string
       (buffer-substring-no-properties
        (point)
        (point-max))
       'utf-8))))

(defun ollama-get-response-from-line (line)
  (cdr
   (assoc 'response
          (json-read-from-string line))))

(defun ollama-prompt (url prompt model)
  (mapconcat 'ollama-get-response-from-line
             (cl-remove-if #'(lambda (str) (string= str "")) 
                        (split-string (ollama-fetch url prompt model) "\n")) ""))

;;;###autoload
(defun ollama-prompt-line ()
  "Prompt with current word."
  (interactive)
  (with-output-to-temp-buffer "*ollama*"
    (princ
     (ollama-prompt ollama:endpoint (thing-at-point 'line) ollama:model))))

;;;###autoload
(defun ollama-define-word ()
  "Find definition of current word."
  (interactive)
  (with-output-to-temp-buffer "*ollama*"
    (princ
     (ollama-prompt ollama:endpoint (format "define %s" (thing-at-point 'word)) ollama:model))))

;;;###autoload
(defun ollama-translate-word ()
  "Translate current word."
  (interactive)
  (with-output-to-temp-buffer "*ollama*"
    (princ
     (ollama-prompt ollama:endpoint (format "translate \"%s\" to %s" (thing-at-point 'word) ollama:language) ollama:model))))

;;;###autoload
(defun ollama-summarize-region ()
  "Summarize marked text."
  (interactive)
  (with-output-to-temp-buffer "*ollama*"
    (princ
     (ollama-prompt ollama:endpoint (format "summarize \"\"\"%s\"\"\"" (buffer-substring (region-beginning) (region-end))) ollama:model))))

;; rewrite this function to apply ollama to chunks of a buffer in a sliding window
;;;###autoload
(defun ollama-exec-region ()
  "Exec marked text."
  (interactive)
  (with-output-to-temp-buffer "*ollama*"
    (princ
     (ollama-prompt ollama:endpoint (format "execute \"\"\"%s\"\"\"" (buffer-substring (region-beginning) (region-end))) ollama:model))))

;; rewrite this function to apply ollama to chunks of a buffer in a sliding window
;;;###autoload
(defun ollama-reinterpret-region-insert ()
  "Exec marked text."
  (interactive)
  (with-output-to-temp-buffer "*ollama*"
    (princ (format "#+begin_src input\nrewrite and reinterpret creatively preserving main ideas \"\"\"%s\"\"\"\n#+end_src\n" (buffer-substring (region-beginning) (region-end))))
    (princ
     (format "#+begin_src output\n%s\n#+end_src\n"
     (ollama-prompt ollama:endpoint (format "rewrite and reinterpret creatively preserving main ideas \"\"\"%s\"\"\"" (buffer-substring (region-beginning) (region-end))) ollama:model))
  )))

(defun ollama-reinterpret-region-insert-2x ()
  "Exec marked text."
  (interactive)
  (with-output-to-temp-buffer "*ollama*"
    (princ (format "#+begin_src input\nrewrite and reinterpret creatively preserving main ideas \"\"\"%s\"\"\"\n#+end_src\n" (buffer-substring (region-beginning) (region-end))))
    (princ
     (format "#+begin_src output\n%s\n#+end_src\n"
     (ollama-prompt ollama:endpoint (format "rewrite and reinterpret creatively preserving main ideas \"\"\"%s\"\"\"" (buffer-substring (region-beginning) (region-end))) ollama:model))
  )))

(defun ollama-exec-region-org ()
  "Exec marked text."
  (interactive)
  (with-output-to-temp-buffer "*ollama*"
    (princ (format "#+begin_src input\nexecute \"\"\"%s\"\"\"\n#+end_src\n" (buffer-substring (region-beginning) (region-end))))
    (princ
     (format "#+begin_src output\n%s\n#+end_src\n"
     (ollama-prompt ollama:endpoint (format "execute \"\"\"%s\"\"\"" (buffer-substring (region-beginning) (region-end))) ollama:model))
  )))

;;;###autoload
(defun ollama-cuda-region ()
  "Exec marked text."
  (interactive)
  (with-output-to-temp-buffer "*ollama*"
    (princ
     (ollama-prompt ollama:endpoint (format "Interpret the following nvidia CUDA code and explain it  \"\"\"%s\"\"\"" (buffer-substring (region-beginning) (region-end))) ollama:model))))



;; ;;###autoload
;; (defun ollama-exec-chunks ()
;;   "Exec marked text."
;;   (interactive)
;;   (let ((buffer (buffer))
;;         (window-size 1000) ; adjust this value as needed
;;         (start 0)
;;         (end -1))
;;     (loop for i from start to end by (+ window-size 1)))
;;       ;; Get the chunk of buffer from the current start and end positions
;;       (let ((chunk (buffer-substring buffer start end))
;;             (prompt (format "execute \"\"%s\"\"" chunk))))
;;         ;; Execute OLlama on the chunk using the specified model
;;         (with-output-to-temp-buffer "*ollama*"
;;           (princ (ollama-prompt ollama:endpoint prompt ollama:model))))
;;       ;; Move the start position forward by the window size
;;       (setq start (+ start window-size)))))

;; ;;;### Review and rewrite this function
;; (defun ollama-exec-region2 ()
;;   "Executes a marked region of text using the OLLAMA API."
;;   (interactive)
;;   (let ((*default-output-buffer* '*olly-output*))
;;     ;; Get the contents of the selected region
;;     ;; Construct the OLLAMA API request string
;;     (let ((request-string (format "execute \"%s\"" (buffer-substring (region-beginning) (region-end)))))
;;       ;; Send the OLLAMA API request and capture the output
;;       (with-output-to-temp-buffer "*olly-output*"
;;         (ollama-prompt ollama:endpoint request-string ollama:model)))))

(defun ollama-reinterpret-region-insert2 ()
  "Execute marked text and save result as string."
  (interactive)
  (with-output-to-temp-buffer "*ollama*"  
    (setq inputd (format "rewrite and reinterpret creatively preserving main ideas: \"\"%s\"\"" (buffer-substring (region-beginning) (region-end))))
    (setq response  (ollama-prompt ollama:endpoint inputd ollama:model))
	(setq inputd2 (format "rewrite and reinterpret creatively preserving main ideas: \"\"%s\"\"" response))
			(princ (format "#+begin_src input\nrewrite and reinterpret creatively preserving main ideas \"\"%s\"\"" inputd ))
			(princ (format "#+begin_src output\n%s\n#+end_src\n" response))
		(dotimes (i 4)
			(setq response (ollama-prompt ollama:endpoint response ollama:model))
			(princ (format "#+begin_src output%s\n%s\n#+end_src\n" i response))
	))

  )

(provide 'ollama)
;;; ollama.el ends here
