;;; agent-shell-command-output-tests.el --- Command output tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)

;;; Code:

(ert-deftest agent-shell-command-output-retains-raw-results-test ()
  "Preserve raw output through status-only and empty-content updates."
  (let* ((state (agent-shell--make-state))
         (output (concat "  **literal**\t\n"
                         (make-string 40000 ?x) "\n")))
    (agent-shell--save-tool-call
     state "shell" '((:kind . "execute") (:command . "printf")))
    (agent-shell--save-command-output
     state `((toolCallId . "shell")
             (rawOutput (formatted_output . ,output))
             (content . [((content (type . "text") (text . "summary")))])))
    (agent-shell--save-command-output
     state '((toolCallId . "shell") (status . "completed")))
    (agent-shell--save-command-output
     state '((toolCallId . "shell") (content . [])))
    (should (equal output (map-nested-elt state '(:tool-calls "shell" :output))))))

(ert-deftest agent-shell-command-output-content-fallback-test ()
  "Read text content when formatted raw output is unavailable."
  (let ((state (agent-shell--make-state)))
    (agent-shell--save-tool-call state "shell" '((:kind . "execute")))
    (agent-shell--save-command-output
     state '((toolCallId . "shell")
             (rawOutput (formatted_output . 42))
             (content . [((content (type . "text") (text . " first\n")))
                         ((content (type . "text") (text . "second\t ")))])))
    (should (equal " first\n\n\nsecond\t "
                   (map-nested-elt state '(:tool-calls "shell" :output))))))

(ert-deftest agent-shell-command-output-terminal-chunks-test ()
  "Append terminal chunks per command and keep them after completion."
  (let ((state (agent-shell--make-state)))
    (agent-shell--save-tool-call state "A" '((:kind . "execute")))
    (agent-shell--save-tool-call state "B" '((:kind . "execute")))
    (agent-shell--save-command-output
     state '((toolCallId . "A")
             (_meta (terminal_output_delta (terminal_id . "A") (data . " first\n")))))
    (agent-shell--save-command-output
     state '((toolCallId . "B")
             (_meta (terminal_output (terminal_id . "B") (data . "other\n")))))
    (agent-shell--save-command-output
     state '((toolCallId . "A")
             (_meta (terminal_output_delta (terminal_id . "A") (data . "second\t ")))))
    (agent-shell--save-command-output
     state '((toolCallId . "A") (status . "completed") (content . [])
             (rawOutput (exit_code . 0))))
    (agent-shell--save-command-output
     state '((toolCallId . "A")
             (_meta (terminal_output_delta (data . 42)))))
    (should (equal " first\nsecond\t "
                   (map-nested-elt state '(:tool-calls "A" :output))))
    (should (equal "other\n" (map-nested-elt state '(:tool-calls "B" :output))))
    (agent-shell--save-command-output
     state '((toolCallId . "A")
             (rawOutput (formatted_output . "complete output"))
             (_meta (terminal_output_delta (data . "duplicate chunk")))))
    (should (equal "complete output"
                   (map-nested-elt state '(:tool-calls "A" :output))))
    (agent-shell--save-command-output
     state '((toolCallId . "A") (rawOutput (formatted_output . ""))
             (content . [((content (type . "text") (text . "text fallback")))])))
    (should (equal "text fallback"
                   (map-nested-elt state '(:tool-calls "A" :output))))))

(ert-deftest agent-shell-command-output-replayed-terminal-result-test ()
  "Capture output supplied with an initial completed tool call."
  (let ((state (agent-shell--make-state)))
    (cl-letf (((symbol-function 'agent-shell--update-fragment) #'ignore)
              ((symbol-function 'agent-shell--cancel-idle-timer) #'ignore)
              ((symbol-function 'agent-shell--refresh-activity-group-header) #'ignore)
              ((symbol-function 'agent-shell--sync-activity-group-fold) #'ignore)
              ((symbol-function 'agent-shell--emit-event) #'ignore)
              ((symbol-function 'agent-shell-make-tool-call-label)
               (lambda (&rest _) '((:status . "done") (:title . "command")))))
      (agent-shell--on-notification
       :state state
       :acp-notification
       '((method . "session/update")
         (params (update (sessionUpdate . "tool_call") (toolCallId . "replay")
                         (title . "printf") (kind . "execute") (status . "completed")
                         (rawInput (command . "printf hello"))
                         (_meta (terminal_output_delta (terminal_id . "replay")
                                                       (data . "hello"))))))))
    (should (equal "hello" (map-nested-elt state '(:tool-calls "replay" :output))))))

(defun agent-shell-command-output-tests--position (id)
  "Find the start of fragment ID in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when-let* ((match (text-property-search-forward
                       'agent-shell-ui-state nil
                       (lambda (_ state)
                         (equal (map-elt state :qualified-id) id)) t)))
      (prop-match-beginning match))))

(ert-deftest agent-shell-command-output-notifications-and-folds-test ()
  "Render results separately and preserve folds through interleaved updates."
  (with-temp-buffer
    (agent-shell-ui-mode 1)
    (let ((state (agent-shell--make-state))
          (agent-shell-activity-group-expand-by-default t)
          (agent-shell-ui-post-expand-fragment-at-point-hook
           '(agent-shell--render-markdown))
          (output "\n\n  **literal**\t\n```text\n# heading\n```\n"))
      (map-put! state :request-count 1)
      (cl-letf (((symbol-function 'agent-shell--update-fragment)
                 (lambda (&rest args)
                   (let ((range
                          (agent-shell-ui-update-fragment
                           (agent-shell-ui-make-fragment-model
                            :namespace-id (map-elt state :request-count)
                            :block-id (plist-get args :block-id)
                            :label-left (plist-get args :label-left)
                            :label-right (plist-get args :label-right)
                            :body (plist-get args :body)
                            :detail (plist-get args :detail)
                            :group-id (plist-get args :group-id)
                            :group-label (plist-get args :group-label)
                            :group-expanded (plist-get args :group-expanded))
                           :expanded (plist-get args :expanded))))
                     (when-let* ((body-start (map-nested-elt range '(:body :start)))
                                 (body-end (map-nested-elt range '(:body :end)))
                                 ((not (agent-shell-ui--body-invisible-p
                                        body-start body-end))))
                       (save-restriction
                         (narrow-to-region body-start body-end)
                         (let ((inhibit-read-only t))
                           (agent-shell--render-markdown)))))))
                ((symbol-function 'agent-shell--append-transcript) #'ignore)
                ((symbol-function 'agent-shell--refresh-activity-group-header) #'ignore)
                ((symbol-function 'agent-shell--delete-fragment) #'ignore)
                ((symbol-function 'agent-shell--cancel-idle-timer) #'ignore)
                ((symbol-function 'agent-shell--emit-event) #'ignore)
                ((symbol-function 'agent-shell-make-tool-call-label)
                 (lambda (&rest _) '((:status . "run") (:title . "command")))))
        (cl-flet ((notify (update)
                    (agent-shell--on-notification
                     :state state
                     :acp-notification
                     `((method . "session/update") (params (update . ,update))))))
          (notify '((sessionUpdate . "tool_call") (toolCallId . "A")
                    (title . "printf") (kind . "execute") (status . "pending")
                    (rawInput (command . "printf hello"))))
          (notify '((sessionUpdate . "tool_call") (toolCallId . "B")
                    (title . "true") (kind . "execute") (status . "pending")
                    (rawInput (command . "true"))))
          (let* ((parent-start (agent-shell-command-output-tests--position "1-A"))
                 (parent (agent-shell-ui--block-range :position parent-start))
                 (detail (agent-shell-ui--detail-range parent)))
            (should (< parent-start (map-elt detail :start)))
            (should (< (map-elt detail :end)
                       (agent-shell-command-output-tests--position "1-B")))
            (should (get-text-property (map-elt detail :start) 'invisible)))
          (notify `((sessionUpdate . "tool_call_update") (toolCallId . "A")
                    (status . "in_progress")
                    (_meta (terminal_output_delta (terminal_id . "A")
                                                  (data . ,(substring output 0 12))))))
          (notify `((sessionUpdate . "tool_call_update") (toolCallId . "A")
                    (_meta (terminal_output_delta (terminal_id . "A")
                                                  (data . ,(substring output 12))))))
          (goto-char (agent-shell-command-output-tests--position "1-A"))
          (should (map-elt (get-text-property (point) 'agent-shell-ui-state) :collapsed))
          (agent-shell-ui-toggle-fragment)
          (let* ((parent (agent-shell-ui--block-range :position (point)))
                 (detail (agent-shell-ui--detail-range parent))
                 (body (agent-shell-ui--nearest-range-matching-property
                        :property 'agent-shell-ui-detail-section :value 'body
                        :from (map-elt detail :start) :to (map-elt detail :end))))
            (should-not (get-text-property (map-elt detail :start) 'invisible))
            (should (get-text-property (map-elt body :start) 'invisible))
            (goto-char (map-elt detail :start))
            (should (eq (lookup-key (get-text-property (point) 'keymap) (kbd "RET"))
                        'agent-shell-ui-toggle-detail))
            (agent-shell-ui-toggle-detail)
            (should-not (get-text-property (map-elt body :start) 'invisible)))
          (notify '((sessionUpdate . "tool_call_update") (toolCallId . "A")
                    (status . "completed") (content . []) (rawOutput (exit_code . 0))))
          (goto-char (agent-shell-command-output-tests--position "1-A"))
          (let* ((parent (agent-shell-ui--block-range :position (point)))
                 (detail (agent-shell-ui--detail-range parent)))
            (should-not (map-elt (get-text-property (map-elt detail :start)
                                                   'agent-shell-ui-detail) :collapsed)))
          (should (string-search output (buffer-substring-no-properties
                                         (point-min) (point-max))))
          (should (equal output (map-nested-elt state '(:tool-calls "A" :output))))
          (agent-shell-ui-toggle-fragment)
          (notify '((sessionUpdate . "tool_call_update") (toolCallId . "A")
                    (rawOutput (formatted_output . "second result\n"))))
          (goto-char (agent-shell-command-output-tests--position "1-A"))
          (let* ((parent (agent-shell-ui--block-range :position (point)))
                 (detail (agent-shell-ui--detail-range parent)))
            (should (get-text-property (map-elt detail :start) 'invisible))
            (should-not (map-elt (get-text-property (map-elt detail :start)
                                                   'agent-shell-ui-detail) :collapsed)))
          (agent-shell-ui-toggle-fragment)
          (goto-char (point-min))
          (search-forward "second result")
          (should-not (get-text-property (match-beginning 0) 'invisible))
          (let ((inhibit-read-only t))
            (agent-shell-ui--set-group-collapsed "1-activity-1" t))
          (notify '((sessionUpdate . "tool_call_update") (toolCallId . "A")
                    (rawOutput (formatted_output . "third result\n"))))
          (goto-char (point-min))
          (search-forward "third result")
          (should (get-text-property (match-beginning 0) 'invisible))
          (let ((inhibit-read-only t))
            (agent-shell-ui--set-group-collapsed "1-activity-1" nil))
          (goto-char (point-min))
          (search-forward "third result")
          (should-not (get-text-property (match-beginning 0) 'invisible))
          (goto-char (agent-shell-command-output-tests--position "1-A"))
          (goto-char (map-elt (agent-shell-ui--detail-range
                              (agent-shell-ui--block-range :position (point))) :start))
          (agent-shell-ui-toggle-detail)
          (notify '((sessionUpdate . "tool_call_update") (toolCallId . "A")
                    (status . "completed")))
          (goto-char (point-min))
          (search-forward "third result")
          (should (get-text-property (match-beginning 0) 'invisible))
          (notify '((sessionUpdate . "tool_call_update") (toolCallId . "B")
                    (status . "completed")))
          (should (string-search "No output supplied by the agent." (buffer-string)))
          (notify '((sessionUpdate . "tool_call") (toolCallId . "read")
                    (title . "Read") (kind . "read") (status . "pending")))
          (notify '((sessionUpdate . "tool_call_update") (toolCallId . "read")
                    (status . "completed")
                    (content . [((content (type . "text") (text . "file contents")))])))
          (should-not (agent-shell-ui--detail-range
                       (agent-shell-ui--block-range
                        :position (agent-shell-command-output-tests--position "1-read")))))))))


(ert-deftest agent-shell-command-output-item-navigation-test ()
  "Visit visible Output headers in both directions and skip hidden ones."
  (with-temp-buffer
    (setq major-mode 'agent-shell-mode)
    (agent-shell-ui-mode 1)
    (agent-shell-ui-update-fragment
     (agent-shell-ui-make-fragment-model
      :namespace-id "nav" :block-id "A" :label-left "command A"
      :body "command text"
      :detail '((:label . "Output") (:body . "result"))
      :group-id "group" :group-label "Activity" :group-expanded t)
     :expanded t :navigation 'always)
    (agent-shell-ui-update-fragment
     (agent-shell-ui-make-fragment-model
      :namespace-id "nav" :block-id "B" :label-left "command B" :body "next")
     :expanded t :navigation 'always)
    (let* ((a (agent-shell-command-output-tests--position "nav-A"))
           (b (agent-shell-command-output-tests--position "nav-B"))
           (group (agent-shell-command-output-tests--position "nav-group"))
           (detail (agent-shell-ui--detail-range
                    (agent-shell-ui--block-range :position a)))
           (header (map-elt detail :start)))
      (cl-letf (((symbol-function 'agent-shell--typing-at-prompt-p) #'ignore)
                ((symbol-function 'comint-next-prompt) #'ignore)
                ((symbol-function 'agent-shell-next-permission-button) #'ignore)
                ((symbol-function 'agent-shell-previous-permission-button) #'ignore))
        (goto-char a)
        (agent-shell-next-item)
        (should (= (point) header))
        (should (map-elt (get-text-property (point) 'agent-shell-ui-detail) :collapsed))
        (agent-shell-next-item)
        (should (= (point) b))
        (agent-shell-previous-item)
        (should (= (point) header))
        (call-interactively (key-binding (kbd "RET")))
        (should-not (map-elt (get-text-property (point) 'agent-shell-ui-detail) :collapsed))
        (agent-shell-next-item)
        (should (= (point) b))
        (agent-shell-previous-item)
        (should (= (point) header))
        (agent-shell-previous-item)
        (should (= (point) a))
        (agent-shell-ui-toggle-fragment)
        (agent-shell-next-item)
        (should (= (point) b))
        (agent-shell-previous-item)
        (should (= (point) a))
        (agent-shell-ui-toggle-fragment)
        (agent-shell-next-item)
        (should (= (point) header))
        (let ((inhibit-read-only t))
          (agent-shell-ui--set-group-collapsed "nav-group" t))
        (goto-char group)
        (agent-shell-next-item)
        (should (= (point) b))
        (agent-shell-previous-item)
        (should (= (point) group))))))

(provide 'agent-shell-command-output-tests)
;;; agent-shell-command-output-tests.el ends here
