;;; gptel-memory.el --- Memory and context management for gptel -*- lexical-binding: t; -*-

;;; Commentary:
;; Hierarchical memory system for gptel sessions
;; - Maintains project context across sessions
;; - Automatic summarization when context grows too large
;; - Smart context requests and loading
;; - Topic-based organization for focused contexts

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

(defun gptel-memory--get-memory-dir (&optional topic)
  "Get or create memory directory in .gptel.
If TOPIC is provided, get topic-specific memory directory."
  (let* ((gptel-dir (gptel-session--get-dir))
         (memory-base (expand-file-name "memory" gptel-dir)))
    (unless (file-exists-p memory-base)
      (make-directory memory-base t))
    (if (and topic (> (length topic) 0))
        (let ((topic-dir (expand-file-name (concat "topics/" topic) memory-base)))
          (unless (file-exists-p topic-dir)
            (make-directory topic-dir t))
          topic-dir)
      memory-base)))

(defun gptel-memory--get-archive-dir (&optional topic)
  "Get or create archive directory in .gptel/memory.
If TOPIC is provided, get topic-specific archive."
  (let ((archive-dir (expand-file-name "archive" (gptel-memory--get-memory-dir topic))))
    (unless (file-exists-p archive-dir)
      (make-directory archive-dir t))
    archive-dir))

(defun gptel-memory--context-file (&optional topic)
  "Return path to context.org file.
If TOPIC is provided, return topic-specific context file."
  (expand-file-name "context.org" (gptel-memory--get-memory-dir topic)))

(defun gptel-memory--index-file (&optional topic)
  "Return path to index.org file.
If TOPIC is provided, return topic-specific index file."
  (expand-file-name "index.org" (gptel-memory--get-memory-dir topic)))

(defun gptel-memory--get-current-topic ()
  "Get current session topic from buffer-local variable or file property."
  (or (bound-and-true-p gptel-session-current-topic)
      (and (buffer-file-name)
           (org-entry-get nil "TOPIC" t))))

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

(defun gptel-memory-init (&optional topic)
  "Initialize memory structure for project.
If TOPIC is provided, initialize topic-specific memory."
  (interactive
   (list (when current-prefix-arg
           (read-string "Topic (empty for general): "))))
  (let* ((memory-dir (gptel-memory--get-memory-dir topic))
         (context-file (gptel-memory--context-file topic))
         (index-file (gptel-memory--index-file topic))
         (project-name (or (and (projectile-project-root)
                               (not (string= (projectile-project-name) "-"))
                               (projectile-project-name))
                          (file-name-nondirectory
                           (directory-file-name (projectile-project-root)))
                          "default"))
         (project-root (or (projectile-project-root) default-directory))
         (timestamp (format-time-string "%Y-%m-%d %H:%M")))

    ;; Create memory directory
    (gptel-memory--get-memory-dir topic)
    (gptel-memory--get-archive-dir topic)

    (message "Initialized memory%s at %s"
             (if topic (format " for topic '%s'" topic) "")
             memory-dir)))
                               (not (string= (projectile-project-name) "-"))
                               (projectile-project-name))
                          (file-name-nondirectory
                           (directory-file-name (projectile-project-root)))
                          "default"))
         (project-root (or (projectile-project-root) default-directory))
         (timestamp (format-time-string "%Y-%m-%d %H:%M")))

    ;; Create memory directory
    (gptel-memory--get-memory-dir)
    (gptel-memory--get-archive-dir)))

;;; Session Analysis and Context Extraction

(defconst gptel-memory-extraction-prompt
  "Analyze this gptel session and extract key information:

%s

Extract the following in a structured format:

* Current Task
What is the main task or goal discussed in this session?

* Recent Changes
What specific changes were made (code, files, configurations)?

* Key Decisions
What important decisions or conclusions were reached?

* Critical Files
Which files were discussed or modified?

* Known Issues
Were any problems, bugs, or issues identified?

* Architecture Notes
Were there any architectural insights, patterns, or design decisions?

Format your response EXACTLY as shown above with these section headings."
  "Prompt template for extracting key points from session.")

(defun gptel-memory--extract-session-content (session-file)
  "Extract and clean session content from SESSION-FILE for analysis."
  (with-temp-buffer
    (insert-file-contents session-file)
    (goto-char (point-min))

    ;; Skip properties and loaded context
    (when (re-search-forward "^#\\+PROPERTY:" nil t)
      (forward-line 1)
      (while (looking-at "^#\\+")
        (forward-line 1)))

    ;; Skip the "Loaded Context" section if present
    (when (re-search-forward "^\\* Loaded Context" nil t)
      (when (re-search-forward "^\\* " nil t)
        (forward-line 0)))

    ;; Get everything from here to end
    (buffer-substring-no-properties (point) (point-max))))

(defun gptel-memory--request-session-analysis (session-file callback)
  "Request AI analysis of SESSION-FILE and call CALLBACK with results."
  (let* ((session-content (gptel-memory--extract-session-content session-file))
         (prompt (format gptel-memory-extraction-prompt session-content))
         (analysis-buffer (get-buffer-create "*gptel-memory-analysis*")))

    (with-current-buffer analysis-buffer
      (erase-buffer)
      (insert prompt)
      (goto-char (point-max))
      (org-mode)
      (gptel-mode 1)

      ;; Send request
      (gptel-request
          prompt
       :buffer analysis-buffer
       :callback
       (lambda (response info)
         (when response
           (funcall callback response)))))))

(defun gptel-memory--parse-analysis (analysis)
  "Parse ANALYSIS response into structured sections.
Returns an alist of (section-name . content) pairs."
  (let ((sections '()))
    (with-temp-buffer
      (insert analysis)
      (goto-char (point-min))

      ;; Parse each section
      (while (re-search-forward "^\\* \\(.*\\)$" nil t)
        (let* ((section-name (match-string 1))
               (start (line-end-position))
               (end (or (and (save-excursion
                              (re-search-forward "^\\* " nil t))
                            (match-beginning 0))
                       (point-max)))
               (content (string-trim (buffer-substring-no-properties start end))))
          (when (> (length content) 0)
            (push (cons section-name content) sections)))))

    (nreverse sections)))
(defun gptel-memory--update-context-section (section-name content &optional topic)
  "Update SECTION-NAME in context.org with CONTENT.
If TOPIC is provided, update topic-specific context."
  (let ((context-file (gptel-memory--context-file topic)))
    (when (file-exists-p context-file)
      (with-temp-buffer
        (insert-file-contents context-file)
        (goto-char (point-min))

        ;; Find the section
        (if (re-search-forward (format "^\\*\\* %s$" (regexp-quote section-name)) nil t)
            (progn
              ;; Found section, replace its content
              (forward-line 1)
              (let ((start (point))
                    (end (or (and (re-search-forward "^\\*\\*\\( \\|$\\)" nil t)
                                 (line-beginning-position))
                            (and (re-search-forward "^\\* " nil t)
                                 (line-beginning-position))
                            (point-max))))
                (delete-region start end)
                (goto-char start)
                (insert content "\n\n")))
          ;; Section not found, try to add it to the right parent section
          (gptel-memory--add-to-parent-section section-name content))

        (write-region nil nil context-file)))))

(defun gptel-memory--add-to-parent-section (section-name content)
  "Add SECTION-NAME with CONTENT to appropriate parent section."
  (let ((parent-mapping '(("Current Task" . "Active Work Context")
                         ("Recent Changes" . "Active Work Context")
                         ("Known Issues" . "Active Work Context")
                         ("Critical Files" . "Code Context")
                         ("Architecture Notes" . "Code Context")
                         ("Dependencies" . "Code Context")
                         ("Key Decisions" . "Project Overview"))))
    (let ((parent (cdr (assoc section-name parent-mapping))))
      (when parent
        (goto-char (point-min))
        (when (re-search-forward (format "^\\* %s$" (regexp-quote parent)) nil t)
          ;; Find end of parent section
          (if (re-search-forward "^\\* " nil t)
              (forward-line 0)
            (goto-char (point-max)))
          ;; Insert new subsection
          (insert (format "\n** %s\n%s\n" section-name content)))))))

(defun gptel-memory--update-conversation-history (session-file timestamp &optional topic)
  "Add SESSION-FILE link to conversation history with TIMESTAMP.
If TOPIC is provided, update topic-specific context."
  (let ((context-file (gptel-memory--context-file topic))
        (session-name (file-name-nondirectory session-file)))
    (with-temp-buffer
      (insert-file-contents context-file)

      ;; Update conversation history section
      (goto-char (point-min))
      (if (re-search-forward "^\\* Conversation History (Recent)" nil t)
          (progn
            (forward-line 1)
            ;; Add entry at the top of conversation history
            (insert (format "\n** %s - [[file:../%s][%s]]\n"
                           timestamp session-name session-name))

            ;; Limit to last 10 sessions
            (let ((count 0))
              (while (and (re-search-forward "^\\*\\* " nil t)
                         (< count 10))
                (setq count (1+ count)))
              (when (re-search-forward "^\\*\\* " nil t)
                (delete-region (line-beginning-position)
                              (or (and (re-search-forward "^\\* " nil t)
                                      (line-beginning-position))
                                  (point-max))))))
        ;; Create conversation history section if missing
        (goto-char (point-max))
        (insert (format "\n* Conversation History (Recent)\n\n** %s - [[file:../%s][%s]]\n"
                       timestamp session-name session-name)))

      (write-region nil nil context-file))))

(defun gptel-memory--update-token-count (&optional topic)
  "Update the CONTEXT_TOKENS property in context.org.
If TOPIC is provided, update topic-specific context."
  (let ((context-file (gptel-memory--context-file topic)))
    (with-temp-buffer
      (insert-file-contents context-file)
      (let ((new-tokens (gptel-memory--estimate-tokens (buffer-string))))
        (goto-char (point-min))
        (if (re-search-forward "^#\\+PROPERTY: CONTEXT_TOKENS \\([0-9]+\\)" nil t)
            (replace-match (format "#+PROPERTY: CONTEXT_TOKENS %d" new-tokens))
          ;; Add property if missing
          (goto-char (point-min))
          (when (re-search-forward "^#\\+PROPERTY: PROJECT_ROOT" nil t)
            (forward-line 1)
            (insert (format "#+PROPERTY: CONTEXT_TOKENS %d\n" new-tokens))))
        (write-region nil nil context-file)
        new-tokens))))

(defun gptel-memory--update-index-statistics (&optional topic)
  "Update statistics in index.org file.
If TOPIC is provided, update topic-specific index."
  (let ((index-file (gptel-memory--index-file topic))
        (context-file (gptel-memory--context-file topic)))
    (when (file-exists-p index-file)
      (with-temp-buffer
        (insert-file-contents index-file)

        ;; Update active context size
        (goto-char (point-min))
        (when (and (file-exists-p context-file)
                  (re-search-forward "^- Active context size: \\([0-9]+\\) tokens" nil t))
          (let ((tokens (with-temp-buffer
                         (insert-file-contents context-file)
                         (gptel-memory--estimate-tokens (buffer-string)))))
            (replace-match (format "- Active context size: %d tokens" tokens))))

        (write-region nil nil index-file)))))

(defun gptel-memory--apply-analysis (analysis session-file &optional topic)
  "Apply parsed ANALYSIS to context.org for SESSION-FILE.
If TOPIC is provided, update topic-specific context."
  (let ((sections (gptel-memory--parse-analysis analysis))
        (timestamp (format-time-string "%Y-%m-%d %H:%M")))

    ;; Update each section
    (dolist (section sections)
      (gptel-memory--update-context-section (car section) (cdr section) topic))

    ;; Update conversation history
    (gptel-memory--update-conversation-history session-file timestamp topic)

    ;; Update token count
    (let ((tokens (gptel-memory--update-token-count topic)))
      (message "Updated context.org (%d tokens)" tokens))

    ;; Update index statistics
    (gptel-memory--update-index-statistics topic)))

;;; Updated session update function

(defun gptel-memory-update-session (session-file)
  "Update memory after session ends with AI-extracted key points."
  (interactive (list (buffer-file-name)))
  (when (and gptel-memory-auto-update session-file)
    (let* ((topic (gptel-memory--get-current-topic))
           (context-file (gptel-memory--context-file topic))
           (index-file (gptel-memory--index-file topic))
           (session-name (file-name-nondirectory session-file))
           (timestamp (format-time-string "%Y-%m-%d"))
           ;; Calculate relative path from index to session
           (relative-path (if topic
                             (concat "../../../sessions/" topic "/" session-name)
                           (concat "../../sessions/" session-name))))

      ;; Initialize memory if needed
      (unless (file-exists-p context-file)
        (gptel-memory-init topic))

      ;; Update index.org with new session
      (when (file-exists-p index-file)
        (with-temp-buffer
          (insert-file-contents index-file)
          (goto-char (point-min))
          (when (re-search-forward "^\\* Sessions by Topic" nil t)
            (forward-line 1)
            (insert (format "** [[file:%s][%s]]: Session\n"
                           relative-path timestamp)))

          ;; Update statistics
          (goto-char (point-min))
          (when (re-search-forward "^- Total sessions: \\([0-9]+\\)" nil t)
            (let ((count (string-to-number (match-string 1))))
              (replace-match (format "- Total sessions: %d" (1+ count)))))

          (write-region nil nil index-file)))

      ;; Request AI analysis and update context
      (message "Analyzing session and updating context%s..."
               (if topic (format " [%s]" topic) ""))
      (gptel-memory--request-session-analysis
       session-file
       (lambda (analysis)
         (gptel-memory--apply-analysis analysis session-file)

         ;; Check if summarization needed after update
         (when (gptel-memory-check-size topic)
           (when (y-or-n-p (format "Context%s exceeds threshold. Summarize now? "
                                  (if topic (format " [%s]" topic) "")))
             (gptel-memory-request-summary topic)))))

      (message "Memory update initiated%s..."
               (if topic (format " [%s]" topic) "")))))

(defun gptel-memory-load (&optional topic)
  "Load context.org into current session buffer.
If TOPIC is provided, load topic-specific context."
  (interactive
   (list (when current-prefix-arg
           (read-string "Topic (empty for current/general): "))))
  (unless topic
    (setq topic (gptel-memory--get-current-topic)))

  (let ((context-file (gptel-memory--context-file topic)))
    (if (file-exists-p context-file)
        (let ((context (with-temp-buffer
                        (insert-file-contents context-file)
                        (buffer-string))))
          (save-excursion
            (goto-char (point-min))
            ;; Skip properties
            (when (re-search-forward "^#\\+PROPERTY:" nil t)
              (while (looking-at "^#\\+")
                (forward-line 1))
              (forward-line 1))
            ;; Insert loaded context section
            (insert (format "\n* Loaded Context%s\n\n"
                           (if topic (format " [%s]" topic) "")))
            (insert "#+begin_quote\n")
            (insert context)
            (insert "\n#+end_quote\n\n"))
          (message "Loaded context%s (%d tokens)"
                   (if topic (format " [%s]" topic) "")
                   (gptel-memory--estimate-tokens context))
          context)
      (message "No context file found%s. Run M-x gptel-memory-init"
               (if topic (format " for topic '%s'" topic) ""))
      nil)))

(defun gptel-memory-check-size (&optional topic)
  "Check if context needs summarization. Returns t if exceeds threshold.
If TOPIC is provided, check topic-specific context."
  (let ((context-file (gptel-memory--context-file topic)))
    (when (file-exists-p context-file)
      (with-temp-buffer
        (insert-file-contents context-file)
        (let* ((tokens (gptel-memory--estimate-tokens (buffer-string)))
               (threshold (* gptel-memory-max-tokens gptel-memory-summarize-threshold)))
          (when (> tokens threshold)
            (message "Context size%s: %d tokens (threshold: %d)"
                     (if topic (format " [%s]" topic) "")
                     tokens threshold)
            t))))))

(defun gptel-memory-request-summary (&optional topic)
  "Send summarization request to model.
If TOPIC is provided, summarize topic-specific context."
  (interactive
   (list (when current-prefix-arg
           (read-string "Topic (empty for current/general): "))))
  (unless topic
    (setq topic (gptel-memory--get-current-topic)))

  (let* ((context-file (gptel-memory--context-file topic))
         (context-content (with-temp-buffer
                            (insert-file-contents context-file)
                            (buffer-string)))
         (prompt (format gptel-memory-summarization-prompt context-content))
         (summary-buffer (get-buffer-create
                         (format "*gptel-memory-summary%s*"
                                (if topic (format "-%s" topic) "")))))

    ;; Create summary request buffer
    (with-current-buffer summary-buffer
      (erase-buffer)
      (insert prompt)
      (goto-char (point-max))
      (org-mode)
      (gptel-mode 1)
      ;; Store topic as buffer-local variable for the callback
      (setq-local gptel-session-current-topic topic)
      (message "Requesting summary from model%s..."
               (if topic (format " [%s]" topic) ""))

      ;; Send request
      (gptel-request
          prompt
        :buffer summary-buffer
        :callback
        (lambda (response info)
          (when response
            (gptel-memory-review-summary response)))))

    (display-buffer summary-buffer)))

(defun gptel-memory-review-summary (summary)
  "Show summarization results for user review."
  (let ((review-buffer (get-buffer-create "*gptel-memory-review*"))
        (topic (gptel-memory--get-current-topic)))
    (with-current-buffer review-buffer
      (erase-buffer)
      (insert "# Review Memory Summary\n")
      (insert "# Mark items with [x] to FORGET them\n")
      (insert "# Press C-c C-c to apply, C-c C-k to cancel\n\n")
      (insert summary)
      (goto-char (point-min))
      (org-mode)

      ;; Store topic as buffer-local variable for apply function
      (setq-local gptel-session-current-topic topic)

      ;; Add local keybindings
      (local-set-key (kbd "C-c C-c") 'gptel-memory-apply-review)
      (local-set-key (kbd "C-c C-k") 'gptel-memory-cancel-review)
      (local-set-key (kbd "C-c C-t") 'org-toggle-checkbox))

    (switch-to-buffer review-buffer)
    (message "Review summary. C-c C-c to apply, C-c C-k to cancel")))

(defun gptel-memory-apply-review ()
  "Apply reviewed summary to context."
  (interactive)
  (let* ((summary (buffer-string))
         (forget-items '())
         (topic (gptel-memory--get-current-topic))
         (context-file (gptel-memory--context-file topic))
         (archive-file (expand-file-name
                       (format "%s.org" (format-time-string "%Y-%m-%d"))
                       (gptel-memory--get-archive-dir topic))))

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

    (message "Applied summary%s. Archived old context to %s"
             (if topic (format " [%s]" topic) "")
             archive-file)
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
  (let ((topic (gptel-memory--get-current-topic)))
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
           (let ((context-file (gptel-memory--context-file topic)))
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

    (message "Added %d context items" (length requests))))

;;; Auto-loading hook

(defun gptel-memory--auto-load-hook ()
  "Hook to auto-load context when starting session."
  (when (and gptel-memory-auto-load
             gptel-mode
             (buffer-file-name)
             (string-match-p "/\\.gptel/sessions/" (buffer-file-name)))
    (let ((topic (gptel-memory--get-current-topic)))
      ;; Initialize memory if needed
      (unless (file-exists-p (gptel-memory--context-file topic))
        (gptel-memory-init topic))
      ;; Load context
      (gptel-memory-load topic))))

(add-hook 'gptel-mode-hook #'gptel-memory--auto-load-hook)

;;; Auto-save hook on session close

(defun gptel-memory--auto-save-on-kill ()
  "Automatically update memory when closing a session buffer."
  (when (and gptel-memory-auto-update
             gptel-mode
             (buffer-file-name)
             (string-match-p "/\\.gptel/sessions/" (buffer-file-name)))
    (gptel-memory-update-session (buffer-file-name))))

(add-hook 'kill-buffer-hook #'gptel-memory--auto-save-on-kill)

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

(defun gptel-memory-status (&optional topic)
  "Show memory system status.
If TOPIC is provided, show status for that topic."
  (interactive
   (list (when current-prefix-arg
           (read-string "Topic (empty for current/general): "))))
  (unless topic
    (setq topic (gptel-memory--get-current-topic)))

  (let* ((context-file (gptel-memory--context-file topic))
         (tokens (when (file-exists-p context-file)
                  (with-temp-buffer
                    (insert-file-contents context-file)
                    (gptel-memory--estimate-tokens (buffer-string)))))
         (threshold (* gptel-memory-max-tokens gptel-memory-summarize-threshold)))
    (message "Memory%s: %s | Size: %d tokens | Threshold: %d | Auto-load: %s"
            (if topic (format " [%s]" topic) "")
            (if (file-exists-p context-file) "initialized" "not initialized")
            (or tokens 0)
            threshold
            (if gptel-memory-auto-load "on" "off"))))

(defun gptel-memory-view-context (&optional topic)
  "Open context.org file for viewing/editing.
If TOPIC is provided, open that topic's context."
  (interactive
   (list (when current-prefix-arg
           (read-string "Topic (empty for current/general): "))))
  (unless topic
    (setq topic (gptel-memory--get-current-topic)))

  (let ((context-file (gptel-memory--context-file topic)))
    (if (file-exists-p context-file)
        (find-file context-file)
      (message "No context file%s. Run M-x gptel-memory-init"
               (if topic (format " for topic '%s'" topic) "")))))

(defun gptel-memory-view-index (&optional topic)
  "Open index.org file for viewing.
If TOPIC is provided, open that topic's index."
  (interactive
   (list (when current-prefix-arg
           (read-string "Topic (empty for current/general): "))))
  (unless topic
    (setq topic (gptel-memory--get-current-topic)))

  (let ((index-file (gptel-memory--index-file topic)))
    (if (file-exists-p index-file)
        (find-file index-file)
      (message "No index file%s. Run M-x gptel-memory-init"
               (if topic (format " for topic '%s'" topic) "")))))

(provide 'gptel-memory)
;;; gptel-memory.el ends here
