(require 'dash)
(require 'netz)

(defun oslg-netz-create-tag-nodes (heading)
  (let ((tags (plist-get heading :tags)))
    (when tags
      (-map (lambda (tag)
	      `(:id ,tag :label "Tag" :display ,tag))
	    tags))))

(defun oslg-netz-create-links-nodes (heading)
  (let ((links (plist-get heading :links)))
    (when links
      (-map (lambda (link)
	      `(:id ,(plist-get link :path) :label "Note"))
	    links))))

(defun oslg-netz-create-parent-node (heading)
  (let ((parent-id (plist-get heading :parentId)))
    (when parent-id
      `(:id ,parent-id :label "Node"))))

(defun oslg-netz-create-file-node (heading)
  (when (plist-get heading :fileName)
    `(:id ,(plist-get heading :fileName)
	  :path ,(plist-get heading :fileName)
	  :label "File"
	  :display ,(file-name-nondirectory (plist-get heading :fileName)))))

(defun oslg-netz-create-heading-node (heading)
  (plist-put heading :label "Note")
  (plist-put heading :display (plist-get heading :title)))

(defun oslg-netz-create-related-nodes-for-heading (heading)
  (-non-nil
   (-> `(,(oslg-netz-create-parent-node heading))
       (append `(,(oslg-netz-create-file-node heading)))
       (append (oslg-netz-create-links-nodes heading))
       (append (oslg-netz-create-tag-nodes heading)))))

(defun oslg-connect-node-type (source target type graph)
  (when (and source target)
    (netz-add-node target graph)
    (netz-connect-nodes
     (netz-get-node (plist-get source :id) graph)
     (netz-get-node (plist-get target :id) graph)
     `(:type ,type)
     graph)))

(defun oslg-netz-add-heading-to-graph (heading graph)
  ;; create heading node
  (netz-add-node (oslg-netz-create-heading-node heading) graph)
  ;; add links
  (-map (lambda (node)
	  (oslg-connect-node-type heading node "LINKS_TO" graph))
	(oslg-netz-create-links-nodes heading))
  ;; add parent
  (oslg-connect-node-type heading (oslg-netz-create-parent-node heading) "CHILD_OF" graph)
  ;; add tags
  (-map (lambda (node)
	  (oslg-connect-node-type heading node "TAGGED" graph))
	(oslg-netz-create-tag-nodes heading))
  (oslg-connect-node-type heading (oslg-netz-create-file-node heading) "IN_FILE" graph))

(provide 'org-super-links-graph-netz)
