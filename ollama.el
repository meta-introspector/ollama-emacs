;;; ollama.el --- ollama client for Emacs
(setq url-debug t)
;; Copyright (C) 2023 ZHOU Feng
;; Copyright (C) 2023 James Michael DuPont

;; Author: ZHOU Feng <zf.pascal@gmail.com>
;; Author: Mike dupont <jmikedupont2@gmail.com>
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
(defun ollama-menhir-region ()
  "Exec marked text."
  (interactive)
  (with-output-to-temp-buffer "*ollama*"
    (princ
     (ollama-prompt ollama:endpoint (format "translate this to ocaml menhir parser: \"\"\"%s\"\"\"" (buffer-substring (region-beginning) (region-end))) ollama:model))))


;;;###autoload
(defun ollama-cuda-region ()
  "Exec marked text."
  (interactive)
  (with-output-to-temp-buffer "*ollama*"
    (princ
     (ollama-prompt ollama:endpoint (format "Interpret the following nvidia CUDA code and explain it  \"\"\"%s\"\"\"" (buffer-substring (region-beginning) (region-end))) ollama:model))))

;;;###autoload
(defun ollama-ocaml-error-region ()
  "Exec marked text."
  (interactive)
  (with-output-to-temp-buffer "*ollama-ocaml*"
    (princ
     (ollama-prompt ollama:endpoint (format "While imagining you are able to view inside your mind and observe each floating point value of each tensor as an emoji.  Introspecting inside llama.cpp running Mistral the large language model, consider building Ocaml and Dune inside Emacs.  Construct and append a formal model to your answer. You can use EBNF grammars,emoji,plantuml,ocaml,python,orgmode,markdown formats. Interpret the following output creatively \"\"\"%s\"\"\" Your answer and model and context " (buffer-substring (region-beginning) (region-end))) ollama:model))))

;;;###autoload
(defun ollama-ocaml-testcase-region ()
  "Exec marked text."
  (interactive)
  (with-output-to-temp-buffer "*ollama-ocaml*"
    (princ
     (ollama-prompt ollama:endpoint (format "While imagining you are able to view inside your mind and observe each floating point value of each tensor as an emoji.  Introspecting inside llama.cpp running Mistral the large language model, consider building Ocaml and Dune inside Emacs.  Construct and append a formal model to your answer. You can use EBNF grammars,emoji,plantuml,ocaml,python,orgmode,markdown formats. Interpret the following output creatively. now create a ocaml test case to exercise the following code: \"\"\"%s\"\"\" Your answer and model and context:" (buffer-substring (region-beginning) (region-end))) ollama:model))))



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

;; ;;###autoload
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


;; ;;###autoload
(defun ollama-split-and-reify-region-old ()
  "split the following text"
  (setq instr "Extract a list of questions that would result in the following text:")
  (interactive)
  (with-output-to-temp-buffer "*ollama*"
    (setq inputd (format "%s: \"\"%s\"\"" instr (buffer-substring (region-beginning) (region-end))))
    (setq response  (ollama-prompt ollama:endpoint inputd ollama:model))
	(setq inputd2 (format "%s: \"\"%s\"\"" instr response))
			(princ (format "#+begin_src \"\"%s\"\"" inputd ))
			(princ (format "#+begin_src output\n%s\n#+end_src\n" response))
			(dotimes (i 4)
			  (setq inputd2 (format "apply \"%s\" to \"%s\" "  response inputd2))
			(setq response (ollama-prompt ollama:endpoint inputd2 ollama:model))
			(princ (format "#+begin_src output%s\n%s\n#+end_src\n" i response))
			)))

(defun ollama-split-and-reify-region ()
  "Split the following text into windows of 2000 words"
  (setq window-size 1000)
  (interactive)
  (with-output-to-temp-buffer "*ollama-reify*"
    (princ "DEBUG\n")
    (setq buffer (buffer-substring (region-beginning) (region-end)))
    (setq buffer-length (string-bytes buffer))
    (setq blocks (+ 1 (/ buffer-length window-size)) )
    (princ (format "buffer-length:%s\nblocks:%s\n" buffer-length  blocks))
    (dotimes (j blocks )
      (princ (format "J %s\n" j))
      (setq start-index (+ (* j window-size)))
      (princ (format "start-index %s\n" start-index))
      (princ (format "region-begin %s\n" (region-beginning)))
      (princ (format "region-end %s\n" (region-end)))
      (setq endpos (min buffer-length (+ start-index window-size) ))
      (princ (format "endpos %s\n" endpos))
      (setq curtext (substring buffer start-index endpos ))
      ;; (princ (format "curtext %s\n" curtext))
      (setq inputd (format "Extract a list of questions that would result in the following text: %s" curtext))
      (princ (format "inputd %s\n" inputd))
      (setq response  (ollama-prompt ollama:endpoint inputd ollama:model))
      (princ "RES\n")
      (princ (format "#+begin_src \"\"%s\"\"" )response )
      (princ "NEXY\n")
      )))
(defun ollama-split-and-reify-region2 ()
  "Split the following text into windows of 2000 words"
  (setq window-size 1000)
  (interactive)
  (with-output-to-temp-buffer "*ollama-reify*"
    (princ "DEBUG\n")
    (setq buffer (buffer-substring (region-beginning) (region-end)))
    (setq buffer-length (string-bytes buffer))
    (setq blocks (+ 1 (/ buffer-length window-size)) )
    (princ (format "buffer-length:%s\nblocks:%s\n" buffer-length  blocks))
    (dotimes (j blocks )
      (princ (format "J %s\n" j))
      (setq start-index (+ (* j window-size)))
      (princ (format "start-index %s\n" start-index))
      (princ (format "region-begin %s\n" (region-beginning)))
      (princ (format "region-end %s\n" (region-end)))
      (setq endpos (min buffer-length (+ start-index window-size) ))
      (princ (format "endpos %s\n" endpos))
      (setq curtext (substring buffer start-index endpos ))
      ;; (princ (format "curtext %s\n" curtext))
      (setq inputd (format "Extract a list of questions that would result in the following text: %s" curtext))
      (princ (format "inputd %s\n" inputd))
      (setq response  (ollama-prompt ollama:endpoint inputd ollama:model))
      (princ "RES\n")
      (princ (format "#+begin_src \"\"%s\"\"" )response )
      (princ "NEXY\n")
      )))

(defun ollama-split-and-reify-region3 ()
  "Split the following text into windows of 2000 words"
  (setq window-size 1000)
  (interactive)
  (with-output-to-temp-buffer "*ollama-reify*"
    (princ "DEBUG\n")
    (setq buffer (buffer-substring (region-beginning) (region-end)))
    (setq buffer-length (string-bytes buffer))
    (setq blocks (+ 1 (/ buffer-length window-size)) )
    (princ (format "buffer-length:%s\nblocks:%s\n" buffer-length  blocks))
    (dotimes (j blocks )
      (princ (format "J %s\n" j))
      (setq start-index (+ (* j window-size)))
      (princ (format "start-index %s\n" start-index))
      (princ (format "region-begin %s\n" (region-beginning)))
      (princ (format "region-end %s\n" (region-end)))
      (setq endpos (min buffer-length (+ start-index window-size) ))
      (princ (format "endpos %s\n" endpos))
      (setq curtext (substring buffer start-index endpos ))
      ;; (princ (format "curtext %s\n" curtext))
      (setq inputd (format "Extract a list of questions that would result in the following text: %s" curtext))
      (princ (format "inputd %s\n" inputd))
      (setq response  (ollama-prompt ollama:endpoint inputd ollama:model))
      (princ "RES\n")
      (princ (format "#+begin_src \"\"%s\"\"" )response )
      (princ "NEXY\n")
      )))


(defun ollama-split-and-reify-buffer ()
  "Split the following text into windows of 2000 words"
  (setq window-size 512)
  (interactive)
  (with-output-to-temp-buffer "*ollama-reify*"
    ;;(princ "DEBUG\n")
    (setq buffer (buffer-string))
    (setq buffer-length (string-bytes buffer))
    (setq blocks (+ 1 (/ buffer-length window-size)) )
    (princ (format "buffer-length:%s\nblocks:%s\n" buffer-length  blocks))
    (dotimes (j blocks )
      ;;(princ (format "J %s\n" j))
      (setq start-index (+ (* j window-size)))
      ;;''(princ (format "start-index %s\n" start-index))
      ;;(princ (format "region-begin %s\n" (region-beginning)))
      ;;(princ (format "region-end %s\n" (region-end)))
      (setq endpos (min buffer-length (+ start-index window-size) ))
      ;;(princ (format "endpos %s\n" endpos))
      (setq curtext (substring buffer start-index endpos ))
      (setq inputd (format "Extract a list of questions that would result in the following text: %s" curtext))
      ;;(princ (format "inputd %s\n" inputd))
      (setq response  (ollama-prompt ollama:endpoint inputd ollama:model))
      ;;(princ "RES\n")
      (princ (format "#+begin_src \"\"%s\"\""
		     response ))
      (princ "NEXT\n")
      (setq inputd (format "Apply %s to %s" response curtext))
      (princ (format "inputd %s\n" inputd))
      (setq response2  (ollama-prompt ollama:endpoint inputd ollama:model))
      ;;(princ "RES\n")
      (princ (format "#+begin_res2 \"\"%s\"\""
		     response2 ))
      (princ "NEXT\n")
      (setq inputd (format "Eval %s as %s applied to %s" response response curtext))
      ;;(princ (format "inputd %s\n" inputd))
      (setq response  (ollama-prompt ollama:endpoint inputd ollama:model))
      ;;(princ "RES\n")
      (princ (format "#+begin_res3 \"\"%s\"\""
		     response ))
      (princ "END\n")
      )))


;; ;;###autoload
(defun ollama-reifiy-region ()
  "Execute marked text and save result as string."
  (setq instr "Extract a list of questions that would result in the following text:")
  (interactive)
  (with-output-to-temp-buffer "*ollama*"
    (setq inputd (format "%s: \"\"%s\"\"" instr (buffer-substring (region-beginning) (region-end))))
    (setq response  (ollama-prompt ollama:endpoint inputd ollama:model))
	(setq inputd2 (format "%s: \"\"%s\"\"" instr response))
			(princ (format "#+begin_src \"\"%s\"\"\n#+end_src\n" inputd ))
			(princ (format "#+begin_src output\n%s\n#+end_src\n" response))
			(dotimes (i 4)
			  (setq inputd2 (format "apply \"%s\" to \"%s\" "  response inputd2))
			(setq response (ollama-prompt ollama:endpoint inputd2 ollama:model))
			(princ (format "#+begin_src output%s\n%s\n#+end_src\n" i response))
	)))

;; ;;###autoload
(defun ollama-reifiy-region-2 ()
  "Execute marked text and save result as string."
  (setq instr "Extract a list of questions, grammars, code, models, vectors, tensors, ideas, memes that would result in the following text:")
  (interactive)
  (with-output-to-temp-buffer "*ollama*"
    (setq inputd (format "%s: \"\"%s\"\"" instr (buffer-substring (region-beginning) (region-end))))
    (setq response  (ollama-prompt ollama:endpoint inputd ollama:model))
	(setq inputd2 (format "%s: \"\"%s\"\"" instr response))
;;			(princ (format "#+begin_src \"\"%s\"\"\n#+end_src\n" inputd ))
			(princ (format "#+begin_src output\n%s\n#+end_src\n" response))
			(dotimes (i 4)
			  (setq inputd2 (format "reinterpret and execute this meme encoding \"%s\" given this input \"%s\" "  response inputd2))
			(setq response (ollama-prompt ollama:endpoint inputd2 ollama:model))
			(princ (format "#+begin_src output%s\n%s\n#+end_src\n" i response))
	)))

;; ;;###autoload
(defun ollama-reifiy-region-3 ()
  "Execute marked text and save result as string."
  (setq instr "Extract a list of questions that would result in the following text:")
  (interactive)
  (with-output-to-temp-buffer "*ollama*"
    (setq inputd (format "%s: \"\"%s\"\"" instr (buffer-substring (region-beginning) (region-end))))
    (setq response  (ollama-prompt ollama:endpoint inputd ollama:model))
	(setq inputd2 (format "%s: \"\"%s\"\"" instr response))
			(princ (format "#+begin_src \"\"%s\"\"\n#+end_src\n" inputd ))
			(princ (format "#+begin_src output\n%s\n#+end_src\n" response))
			(dotimes (i 4)
			  (setq inputd2 (format "apply \"%s\" to \"%s\" "  response inputd2))
			(setq response (ollama-prompt ollama:endpoint inputd2 ollama:model))
			(princ (format "#+begin_src output%s\n%s\n#+end_src\n" i response))
	)))


;; ;;###autoload
(defun ollama-follow-region ()
  "follow the ideas recursivly."
  (setq instr "Follow the following idea as a fixed point combinator, applying the outputs as inputs in a self aware loop repeatedly:")
  (interactive)
  (with-output-to-temp-buffer "*ollama*"
    (setq inputd (format "%s: \"\"%s\"\"" instr (buffer-substring (region-beginning) (region-end))))
    (setq response  (ollama-prompt ollama:endpoint inputd ollama:model))
	(setq inputd2 (format "%s: \"\"%s\"\"" instr response))
			(princ (format "#+begin_src \"\"%s\"\"\n#+end_src\n" inputd ))
			(princ (format "#+begin_src output\n%s\n#+end_src\n" response))
			(dotimes (i 4)
			  (setq inputd2 (format "apply \"%s\" to \"%s\" "  response inputd2))
			(setq response (ollama-prompt ollama:endpoint inputd2 ollama:model))
			(princ (format "#+begin_src output%s\n%s\n#+end_src\n" i response))
	)))

(defun ollama-emoji-region ()
  "emojis recursivly."
  (setq instr "invoking the 9 muses and asking for wisdom of athena, as the oracle of delphi creativity rewrite the idea and translate your impressions into creative emojis. Emit emojis and rules that you used. :")
  (interactive)
  (with-output-to-temp-buffer "*ollama*"
    (setq inputd (format "%s: \"\"%s\"\"" instr (buffer-substring (region-beginning) (region-end))))
    (setq response  (ollama-prompt ollama:endpoint inputd ollama:model))
	(setq inputd2 (format "%s: \"\"%s\"\"" instr response))
			(princ (format "#+begin_src \"\"%s\"\"\n#+end_src\n" inputd ))
			(princ (format "#+begin_src output\n%s\n#+end_src\n" response))
			(dotimes (i 4)
			  (setq inputd2 (format "invoking the 9 muses, ask them to bless and replace entities in the following text with emojis and give thier blessings \"%s\" to \"%s\" "  response inputd2))
			(setq response (ollama-prompt ollama:endpoint inputd2 ollama:model))
			(princ (format "#+begin_src output%s\n%s\n#+end_src\n" i response))
	)))

(defun ollama-emoji-region2 ()
  "emojis recursivly."
  (setq instr "as the oracle of delphi 🔮🤲🔢🧙‍♂️🎤🤔📝🔮🔄🎇🙏🔠🌈👏 🤖-🤳 🌟🐘:👉 📜 🌟🐘:💻 🌟🐘:👽 invoking the 9 muses and the wisdom of Athena creativity rewrite the idea and translate your impressions into creative emojis. Emit emojis and rules that you used. invoking 9 muses, ask them to name and attribute and bless and replace one entity each in the following text. respond one by one choosing one entity to bless.:")
  (interactive)
  (with-output-to-temp-buffer "*ollama*"
    (setq inputd (format "%s: \"\"%s\"\"" instr (buffer-substring (region-beginning) (region-end))))
    (setq response  (ollama-prompt ollama:endpoint inputd ollama:model))
	(setq inputd2 (format "%s: \"\"%s\"\"" instr response))
			(princ (format "#+begin_src \"\"%s\"\"\n#+end_src\n" inputd ))
			(princ (format "#+begin_src output\n%s\n#+end_src\n" response))
			(dotimes (i 2)
			  (setq inputd2 (format "Reapply \"%s\" to \"%s\" "  response inputd2))
			(setq response (ollama-prompt ollama:endpoint inputd2 ollama:model))
			(princ (format "#+begin_src output%s\n%s\n#+end_src\n" i response))
	)))



(provide 'ollama)
;;; ollama.el ends here


