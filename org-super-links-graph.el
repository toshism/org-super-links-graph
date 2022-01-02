;; heading plist structure
(require 'org-super-links-graph-netz)

(defvar *osl-graph-name* :oslg)

(defun osl-graph-start-netz ()
  (interactive)
  (unless (netz-get-graph *osl-graph-name*)
    (netz-make-graph *osl-graph-name*))
  (save-excursion
    (widen)
    (outline-show-all)
    (beginning-of-buffer)
    ;; make sure we're on the first heading
    (if (or
	 (and (eq (point) (point-min)) (org-before-first-heading-p))
	 (not (eq (point) (point-min))))
	(org-next-visible-heading 1))
    (while (not (osl-graph-end-of-buffer))
      (oslg-netz-add-heading-to-graph (osl-graph-parse-heading) *osl-graph-name*)
      (org-next-visible-heading 1)))
  (message "Done! %s nodes and %s edges." (ht-size (netz-get-nodes *osl-graph-name*)) (ht-size (netz-get-edges *osl-graph-name*))))

;; parse heading into plist
(defun osl-graph-parse-heading ()
  "parse current heading and return plist of needed properties"
  ;; this is dumb and overly verbose but i want it clear and readable first
  (interactive)
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (let ((hplist (list
		     :id nil
		     :tags nil
		     :links nil
		     :parentId nil
		     :title nil
		     :fileName nil
		     :createdDate nil
		     :scheduled nil
		     :deadline nil
		     :bodyText nil)))
	(plist-put hplist :title (osl-graph-get-title))
	(plist-put hplist :id (osl-graph-get-id))
	(plist-put hplist :tags (osl-graph-get-tags))
	(plist-put hplist :links (osl-graph-get-links))
	(plist-put hplist :parentId (osl-graph-get-parentId))
	(plist-put hplist :fileName (osl-graph-get-fileName))
	(plist-put hplist :createdDate (osl-graph-get-createdDate))
	(plist-put hplist :scheduled (osl-graph-get-scheduled))
	(plist-put hplist :deadline (osl-graph-get-deadline))
	(plist-put hplist :bodyText (osl-graph-get-bodyText))
	(widen)
	hplist))))

(defun osl-graph-get-title ()
  (save-excursion
    (org-back-to-heading)
    (org-element-property :title (org-element-at-point))))

(defun osl-graph-get-id ()
  (org-id-get-create))

(defun osl-graph-get-tags ()
  (delete-dups (org-get-tags nil t)))

(defun osl-graph-get-links ()
  (save-excursion
    (org-narrow-to-current-heading)
    (delete-dups (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
	;; should handle other types of links too, but for now just id
	(when (string= (org-element-property :type link) "id")
	`(:type ,(org-element-property :type link)
		:path ,(org-element-property :path link))))))))

(defun osl-graph-get-parentId ()
  (save-excursion
    (save-restriction
      (widen)
      (when (org-up-heading-safe)
	(org-id-get)))))

(defun osl-graph-get-fileName ()
  buffer-file-truename)

(defun osl-graph-get-createdDate ()
  (org-entry-get (point) "CREATED"))

(defun osl-graph-get-scheduled ())
(defun osl-graph-get-deadline ())
(defun osl-graph-get-bodyText ())

(defun osl-graph-add-current-heading ()
  (interactive)
  (oslg-netz-add-heading-to-graph (osl-graph-parse-heading) *osl-graph-name*)
  (message "Done"))

;;; utilities
(defun org-narrow-to-current-heading ()
  "narrow to just the current heading, excluding subtrees"
  (org-narrow-to-subtree)
  (save-excursion
    (org-next-visible-heading 1)
    (narrow-to-region (point-min) (point))))

(defun osl-graph--alpha-string (str)
  (concat "oslg" (replace-regexp-in-string "[^[:alnum:]]" "" str)))

(defun osl-graph-check-if-last-heading ()
  (save-excursion
    (org-next-visible-heading 1)
    (eq (point) (point-max))))

(defun osl-graph-end-of-buffer ()
  (eq (point) (point-max)))


(defun osl-graph--dumb-set-dump (hplist)
  "dumb way to get the list for set statement"
  (format "{%s:'%s', %s:'%s', %s:'%s'}"
	  "orgId" (plist-get hplist :id)
	  "title" (replace-regexp-in-string "'" "\\\\'" (plist-get hplist :title))
	  "createdDate" (plist-get hplist :createdDate)))

;; org protocol
;; (require 'org-protocol)
;; (setq org-protocol-protocol-alist
;;       '(("open-link"
;;          :protocol "open-link"
;;          :function osl-graph--open-from-protocol)))

;; (defun osl-graph--open-from-protocol (info)
;;   (let ((id (plist-get info :id))
;; 	(name (plist-get info :name)))
;;     (if id
;; 	(org-id-goto id)
;;       (org-tags-view nil name))))

(provide 'org-super-links-graph)
