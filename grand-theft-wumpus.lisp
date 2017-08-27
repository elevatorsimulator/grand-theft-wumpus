(load "graph-util.lisp")

(defvar *congestion-city-nodes*)
(defvar *congestion-city-edges*)
(defvar *visited-nodes*)
(defvar *player-pos*)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)


(defun random-node ()
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  (unless (eql a b)
    (list (cons a b) (cons b a))))

(defun make-edges ()
  (remove-duplicates
    (apply #'append (loop repeat *edge-num*
                        collect (edge-pair (random-node) (random-node))))
    :test #'equalp))

(defun direct-edges (node edges)
  (remove-if-not (lambda (edge)
                   (eql node (car edge)))
                 edges))

(defun get-connected (node edges)
  (let ((visited ()))
    (labels ((traverse (node)
               (when (not (member node visited))
                 (push node visited)
                 (mapc #'traverse
                       (mapcar #'cdr (direct-edges node edges))))))
      (traverse node)
      visited)))

(defun find-islands (nodes edges)
  (let ((islands ()))
    (labels ((find-island (nodes)
               (when nodes
                 (let* ((connected (get-connected (car nodes) edges))
                        (disconnected (set-difference nodes connected)))
                   (push connected islands)
                   (when disconnected (find-island disconnected))))))
      (find-island nodes)
      islands)))

(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
            (connect-with-bridges (cdr islands)))))

(defun connect-all-islands (nodes edges)
  (append edges
          (connect-with-bridges (find-islands nodes edges))))

(defun edges-to-alist (nodes edges)
  (mapcar (lambda (node)
            (cons node
                  (mapcar (lambda (edge)
                            (list (cdr edge)))
                          (direct-edges node edges))))
          nodes))

(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
            (let ((node1 (car x))
                  (edges (cdr x)))
              (cons node1
                    (mapcar (lambda (edge)
                              (let ((node2 (car edge)))
                                (if (intersection (edge-pair node1 node2)
                                                  edges-with-cops
                                                  :test #'equalp)
                                    (list node2 'cops)
                                    edge)))
                            edges))))
          edge-alist))

(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num* collect i))
         (edges (connect-all-islands nodes (make-edges)))
         (cops (remove-if-not (lambda (x)
                                (declare (ignore x))
                                (zerop (random *cop-odds*)))
                              edges)))
    (add-cops (edges-to-alist nodes edges) cops)))

(defun neighbours (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist))))

(defun within-one (node1 node2 edge-alist)
  (member node2 (neighbours node1 edge-alist)))

(defun within-two (node1 node2 edge-alist)
  (or (within-one node1 node2 edge-alist)
      (some (lambda (node) (within-one node node2 edge-alist))
            (neighbours node1 edge-alist))))

(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
        (glow-worms (do ((worms () (adjoin (random-node) worms)))
                        ((= (length worms) *worm-num*) worms))))
    (loop for node from 1 to *node-num*
          collect (append (list node)
                          (cond ((eql node wumpus) '(wumpus))
                                ((within-two node wumpus edge-alist) '(blood)))
                          (cond ((member node glow-worms) '(glow-worm))
                                ((intersection glow-worms (neighbours node edge-alist)) '(lights!)))
                          (when (some (lambda (edge) (member 'cops edge))
                                      (cdr (assoc node edge-alist)))
                            '(sirens!))))))

(defun find-empty-node ()
  (loop (let ((node (random-node)))
          (unless (cdr (assoc node *congestion-city-nodes*))
            (return node)))))

(defun known-city-nodes ()
  (mapcar (lambda (node)
            (cons node
                  (if (member node *visited-nodes*)
                      (let ((desc (cdr (assoc node *congestion-city-nodes*))))
                        (if (eql node *player-pos*)
                            (append desc '(*))
                            desc))
                      '(?))))
          (reduce #'union
                  (mapcar (lambda (node) (neighbours node *congestion-city-edges*))
                          *visited-nodes*)
                  :initial-value *visited-nodes*)))

(defun known-city-edges ()
  (mapcar (lambda (node)
            (cons node
                  (mapcar (lambda (edge)
                            (if (member (car edge) *visited-nodes*)
                                edge
                                (remove 'cops edge)))
                          (cdr (assoc node *congestion-city-edges*)))))
          *visited-nodes*))

(defun draw-known-city ()
  (ugraph->png (known-city-nodes) (known-city-edges) "known-city"))

(defun draw-city ()
  (ugraph->png *congestion-city-nodes* *congestion-city-edges* "city"))

(defun handle-new-place (pos edge charging)
  (let* ((node (assoc pos *congestion-city-nodes*))
         (attacked-by-worms (and (member 'glow-worm node)
                                 (not (member pos *visited-nodes*)))))
    (pushnew pos *visited-nodes*)
    (setf *player-pos* pos)
    (draw-known-city)
    (cond ((member 'cops edge) (princ "You ran into the cops. Game over."))
          ((member 'wumpus node) (if charging
                                     (princ "You found the Wumpus!")
                                     (princ "You ran into the Wumpus!")))
          (charging (princ "You wasted your last bullet. Game Over."))
          (attacked-by-worms (let ((new-pos (random-node)))
                               (princ "You ran into a Glow Worm Gang! You're now at ")
                               (princ new-pos)
                               (handle-new-place new-pos nil nil))))))

(defun handle-direction (pos charging)
  (let ((edge (assoc pos
                     (cdr (assoc *player-pos*
                                 *congestion-city-edges*)))))
    (if edge
        (handle-new-place pos edge charging)
        (princ "There's no such location visible from here!"))))

(defun walk (pos)
  (handle-direction pos nil))

(defun charge (pos)
  (handle-direction pos t))

(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city)
  (draw-known-city))
