;;; gptel-memory.el --- Memory and context management for gptel -*- lexical-binding: t; -*-

;;; Commentary:
;; Hierarchical memory system for gptel sessions
;; - Maintains project context across sessions
;; - Automatic summarization when context grows too large
;; - Smart context requests and loading

;;; Code:

(require 'gptel)
(require 'projectile)

;;; Configuration

(defvar gptel-memory-max-tokens 50000
  "Maximum tokens in active context before summarization.")

(defvar gptel-memory-auto-load t
  "Automatically load context when starting session.")

(defvar gptel-memory-auto-update t
  "Automatically update context after session.")

(defvar gptel-memory-smart-context t
  "Enable smart context requests from model.")

(defvar gptel-memory-summarize-threshold 0.8
  "Trigger summarization at 80% of max tokens.")

;;; Helper Functions

(defun gptel-memory--get-memory-dir ()
  "Get or create memory directory in .gptel."
  (let* ((gptel-dir (gptel-session--get-dir))
         (memory-dir (expand-file-name "memory" gptel-dir)))
    (unless (file-exists-p memory-dir)
      (make-directory memory-dir t))
    memory-dir))

(defun gptel-memory--get-archive-dir ()
  "Get or create archive directory in .gptel/memory."
  (let ((archive-dir (expand-file-name "archive" (gptel-memory--get-memory-dir))))
    (unless (file-exists-p archive-dir)
      (make-directory archive-dir t))
    archive-dir))

(defun gptel-memory--context-file ()
  "Return path to context.org file."
  (expand-file-name "context.org" (gptel-memory--get-memory-dir)))

(defun gptel-memory--index-file ()
  "Return path to index.org file."
  (expand-file-name "index.org" (gptel-memory--get-memory-dir)))

(defun gptel-memory--estimate-tokens (text)
  "Rough token estimate: ~4 chars per token."
  (/ (length text) 4))

;;; Context Templates

(defconst gptel-memory-context-template
  "#+TITLE: GPtel Context - %s
#+DATE: %s
#+PROPERTY: PROJECT_ROOT %s
#+PROPERTY: CONTEXT_TOKENS 0

* Project Overview
** Purpose


** Current Goals


** Key Decisions


* Active Work Context
** Current Task


** Recent Changes


** Known Issues


* Code Context
** Critical Files


** Architecture Notes


** Dependencies


* Conversation History (Recent)


* Archive References

"
  "Template for new context.org file.")

(defconst gptel-memory-index-template
  "#+TITLE: GPtel Memory Index
#+DATE: %s

* Sessions by Topic

* Quick Links
- [[file:context.org][Current Context]]
- [[file:archive/][Archived Sessions]]

* Statistics
- Total sessions: 0
- Active context size: 0 tokens
- Last summarization: Never
"
  "Template for new index.org file.")

(defconst gptel-memory-summarization-prompt
  "Please analyze this project context and create a concise summary:

%s

Your task:
1. *Essential Context*: What must be remembered? (decisions, constraints, patterns)
2. *Active Work*: What are we currently working on?
3. *Optional Details*: What details can be safely forgotten?

Format your response EXACTLY as:

* Essential Context
- Fact 1
- Fact 2

* Active Work
- Current task 1
- Current task 2

* Optional Details
- [FORGET?] Detail 1: Description
- [FORGET?] Detail 2: Description
"
  "Prompt template for summarization.")

;;; Core Functions

(defun gptel-memory-init ()
  "Initialize memory structure for project."
  (interactive)
  (let* ((memory-dir (gptel-memory--get-memory-dir))
         (context-file (gptel-memory--context-file))
         (index-file (gptel-memory--index-file))
         (project-name (or (and (projectile-project-root)
                               (not (string= (projectile-project-name) "-"))
                               (projectile-project-name))
                          (file-name-nondirectory 
                           (directory-file-name (projectile-project-root)))
                          "default"))
         (project-root (or (projectile-project-root) default-directory))
         (timestamp (format-time-string "%Y-%m-%d %H:%M")))
    
    ;; Create memory directory
    (gptel-memory--get-memory-dir)
    (gptel-memory--get-archive-dir)
    
    ;; Create context.org if doesn't exist
    (unless (file-exists-p context-file)
      (with-temp-file context-file
        (insert (format gptel-memory-context-template
                       project-name
                       timestamp
                       project-root))))
    
    ;; Create index.org if doesn't exist
    (unless (file-exists-p index-file)
      (with-temp-file index-file
        (insert (format gptel-memory-index-template timestamp))))
    
    (message "Memory initialized at %s" memory-dir)))

(defun gptel-memory-load ()
  "Load context when starting session."
  (interactive)
  (let ((context-file (gptel-memory--context-file)))
    (if (file-exists-p context-file)
        (with-temp-buffer
          (insert-file-contents context-file)
          (let ((context (buffer-string)))
            ;; Insert context at beginning of session
            (save-excursion
              (goto-char (point-min))
              ;; Find Session heading or create one
              (if (re-search-forward "^\\* Session" nil t)
                  (progn
                    (forward-line 0)
                    (insert "\n* Loaded Context\n\n")
                    (insert "#+begin_quote\n")
                    (insert context)
                    (insert "\n#+end_quote\n\n"))
                ;; No session heading, insert after properties
                (goto-char (point-max))
                (insert "\n* Loaded Context\n\n")
                (insert "#+begin_quote\n")
                (insert context)
                (insert "\n#+end_quote\n\n")))
            (message "Loaded context (%d tokens)" 
                    (gptel-memory--estimate-tokens context))
            context))
      (message "No context file found. Run M-x gptel-memory-init")
      nil)))

(defun gptel-memory-check-size ()
  "Check if context needs summarization. Returns t if exceeds threshold."
  (let ((context-file (gptel-memory--context-file)))
    (when (file-exists-p context-file)
      (with-temp-buffer
        (insert-file-contents context-file)
        (let* ((tokens (gptel-memory--estimate-tokens (buffer-string)))
               (threshold (* gptel-memory-max-tokens gptel-memory-summarize-threshold)))
          (when (> tokens threshold)
            (message "Context size: %d tokens (threshold: %d)" tokens threshold)
            t))))))

(defun gptel-memory-update-session (session-file)
  "Update memory after session ends."
  (interactive (list (buffer-file-name)))
  (when (and gptel-memory-auto-update session-file)
    (let* ((context-file (gptel-memory--context-file))
           (index-file (gptel-memory--index-file))
           (session-name (file-name-nondirectory session-file))
           (timestamp (format-time-string "%Y-%m-%d")))
      
      ;; Update index.org with new session
      (when (file-exists-p index-file)
        (with-temp-buffer
          (insert-file-contents index-file)
          (goto-char (point-min))
          (when (re-search-forward "^\\* Sessions by Topic" nil t)
            (forward-line 1)
            (insert (format "** [[file:../../%s][%s]]: Session\n" 
                           session-name timestamp)))
          
          ;; Update statistics
          (goto-char (point-min))
          (when (re-search-forward "^- Total sessions: \\([0-9]+\\)" nil t)
            (let ((count (string-to-number (match-string 1))))
              (replace-match (format "- Total sessions: %d" (1+ count)))))
          
          (write-region nil nil index-file)))
      
      ;; Check if summarization needed
      (when (gptel-memory-check-size)
        (when (y-or-n-p "Context exceeds threshold. Summarize now? ")
          (gptel-memory-request-summary)))
      
      (message "Updated memory index"))))

(defun gptel-memory-request-summary ()
  "Send summarization request to model."
  (interactive)
  (let* ((context-file (gptel-memory--context-file))
         (context-content (with-temp-buffer
                           (insert-file-contents context-file)
                           (buffer-string)))
         (prompt (format gptel-memory-summarization-prompt context-content))
         (summary-buffer (get-buffer-create "*gptel-memory-summary*")))
    
    ;; Create summary request buffer
    (with-current-buffer summary-buffer
      (erase-buffer)
      (insert prompt)
      (goto-char (point-min))
      (org-mode)
      (gptel-mode 1)
      (message "Requesting summary from model...")
      
      ;; Send request
      (gptel-request
       :buffer summary-buffer
       :callback
       (lambda (response info)
         (when response
           (gptel-memory-review-summary response)))))
    
    (display-buffer summary-buffer)))

(defun gptel-memory-review-summary (summary)
  "Show summarization results for user review."
  (let ((review-buffer (get-buffer-create "*gptel-memory-review*")))
    (with-current-buffer review-buffer
      (erase-buffer)
      (insert "# Review Memory Summary\n")
      (insert "# Mark items with [x] to FORGET them\n")
      (insert "# Press C-c C-c to apply, C-c C-k to cancel\n\n")
      (insert summary)
      (goto-char (point-min))
      (org-mode)
      
      ;; Add local keybindings
      (local-set-key (kbd "C-c C-c") 'gptel-memory-apply-review)
      (local-set-key (kbd "C-c C-k") 'gptel-memory-cancel-review)
      (local-set-key (kbd "C-c C-t") 'org-toggle-checkbox))
    
    (switch-to-buffer review-buffer)
    (message "Review summary. C-c C-c to apply, C-c C-k to cancel")))

(defun gptel-memory-apply-review ()
  "Apply reviewed summary to context."
  (interactive)
  (let ((summary (buffer-string))
        (forget-items '())
        (context-file (gptel-memory--context-file))
        (archive-file (expand-file-name 
                      (format "%s.org" (format-time-string "%Y-%m-%d"))
                      (gptel-memory--get-archive-dir))))
    
    ;; Parse forget items
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^- \\[x\\] \\(.*\\)$" nil t)
        (push (match-string 1) forget-items)))
    
    ;; Archive old context
    (when (file-exists-p context-file)
      (copy-file context-file archive-file t))
    
    ;; Create new compressed context
    (with-temp-buffer
      (insert summary)
      
      ;; Remove forgotten items
      (goto-char (point-min))
      (while (re-search-forward "^- \\[x\\] .*$" nil t)
        (delete-region (line-beginning-position) (1+ (line-end-position))))
      
      ;; Remove checkboxes from kept items
      (goto-char (point-min))
      (while (re-search-forward "^- \\[ \\] " nil t)
        (replace-match "- "))
      
      ;; Add header
      (goto-char (point-min))
      (insert (format "#+TITLE: GPtel Context - %s\n"
                     (or (projectile-project-name) "default")))
      (insert (format "#+DATE: %s\n" (format-time-string "%Y-%m-%d %H:%M")))
      (insert (format "#+PROPERTY: PROJECT_ROOT %s\n\n"
                     (or (projectile-project-root) default-directory)))
      
      (write-region nil nil context-file))
    
    (message "Applied summary. Archived old context to %s" archive-file)
    (kill-buffer)))

(defun gptel-memory-cancel-review ()
  "Cancel summary review."
  (interactive)
  (when (y-or-n-p "Cancel summarization? ")
    (kill-buffer)
    (message "Summarization cancelled")))

(defun gptel-memory-parse-context-request (response)
  "Parse model's context request from RESPONSE.
Returns list of (type . identifier) pairs."
  (let ((requests '()))
    (with-temp-buffer
      (insert response)
      (goto-char (point-min))
      
      ;; Look for NEED_CONTEXT: blocks
      (when (re-search-forward "NEED_CONTEXT:" nil t)
        (while (re-search-forward "^- \\(file\\|memory\\|function\\): \\(.+\\)$" nil t)
          (let ((type (intern (match-string 1)))
                (identifier (string-trim (match-string 2))))
            (push (cons type identifier) requests)))))
    
    (nreverse requests)))

(defun gptel-memory-fulfill-request (requests)
  "Automatically fulfill context REQUESTS.
REQUESTS is a list of (type . identifier) pairs."
  (dolist (request requests)
    (let ((type (car request))
          (identifier (cdr request)))
      (pcase type
        ('file
         ;; Add file to context
         (let ((content (gptel-tool-read-file identifier t)))
           (insert (format "\n** Context: %s\n\n" identifier))
           (insert "#+begin_src\n")
           (insert content)
           (insert "\n#+end_src\n\n")))
        
        ('memory
         ;; Add memory section
         (let ((context-file (gptel-memory--context-file)))
           (when (file-exists-p context-file)
             (with-temp-buffer
               (insert-file-contents context-file)
               (goto-char (point-min))
               (when (re-search-forward (format "^\\*+ %s" identifier) nil t)
                 (let ((start (line-beginning-position))
                       (end (or (and (re-search-forward "^\\*+ " nil t)
                                    (line-beginning-position))
                               (point-max))))
                   (insert (format "\n** Context: %s\n\n" identifier))
                   (insert (buffer-substring start end))
                   (insert "\n")))))))
        
        ('function
         ;; Search for function definition
         (message "Function context not yet implemented: %s" identifier)))))
  
  (message "Added %d context items" (length requests)))

;;; Auto-loading hook

(defun gptel-memory--auto-load-hook ()
  "Hook to auto-load context when starting session."
  (when (and gptel-memory-auto-load
             gptel-mode
             (buffer-file-name)
             (string-match-p "/\\.gptel/session-" (buffer-file-name)))
    ;; Initialize memory if needed
    (unless (file-exists-p (gptel-memory--context-file))
      (gptel-memory-init))
    ;; Load context
    (gptel-memory-load)))

(add-hook 'gptel-mode-hook #'gptel-memory--auto-load-hook)

;;; Response parsing hook

(defun gptel-memory--parse-response-hook (response info)
  "Parse RESPONSE for context requests."
  (when gptel-memory-smart-context
    (let ((requests (gptel-memory-parse-context-request response)))
      (when requests
        (when (y-or-n-p (format "Model requests %d context items. Add them? " 
                               (length requests)))
          (gptel-memory-fulfill-request requests))))))

(add-hook 'gptel-post-response-functions #'gptel-memory--parse-response-hook)

;;; Interactive Commands

(defun gptel-memory-status ()
  "Show memory system status."
  (interactive)
  (let* ((context-file (gptel-memory--context-file))
         (tokens (when (file-exists-p context-file)
                  (with-temp-buffer
                    (insert-file-contents context-file)
                    (gptel-memory--estimate-tokens (buffer-string)))))
         (threshold (* gptel-memory-max-tokens gptel-memory-summarize-threshold)))
    (message "Memory: %s | Size: %d tokens | Threshold: %d | Auto-load: %s"
            (if (file-exists-p context-file) "initialized" "not initialized")
            (or tokens 0)
            threshold
            (if gptel-memory-auto-load "on" "off"))))

(defun gptel-memory-view-context ()
  "Open context.org file for viewing/editing."
  (interactive)
  (let ((context-file (gptel-memory--context-file)))
    (if (file-exists-p context-file)
        (find-file context-file)
      (message "No context file. Run M-x gptel-memory-init"))))

(defun gptel-memory-view-index ()
  "Open index.org file for viewing."
  (interactive)
  (let ((index-file (gptel-memory--index-file)))
    (if (file-exists-p index-file)
        (find-file index-file)
      (message "No index file. Run M-x gptel-memory-init"))))

(provide 'gptel-memory)
;;; gptel-memory.el ends here
