(in-package #:syshack)

(defstruct dialogue
  from 
  message)

(defstruct vertex
  id         ;; 정점의 고유 ID
  label      ;; 정점에 표시할 대사
  edges      ;; 정점에서 나가는 간선의 리스트
  required-p ;; 해당 정점이 반드시 지나야하는 곳인지 여부 
  visited-p  ;; 해당 정점이 방문되었는지 여부 
)

(defstruct edge
  to        ;; 연결되는 정점의 ID
  label)    ;; 간선의 레이블 (선택지 등)

(defstruct graph
  vertices) ;; 그래프의 모든 정점을 포함하는 리스트

(defun make-empty-graph ()
  "빈 그래프를 생성합니다."
  (make-graph :vertices '()))

(defun add-vertex (graph &key id label (required-p nil) (visited-p nil))
  "그래프에 정점을 추가합니다."
  (push (make-vertex :id id :label label :edges '() :required-p required-p :visited-p visited-p) 
	(graph-vertices graph)))

(defun add-edge (graph from-id to-id label)
  "그래프에서 특정 정점 사이에 간선을 추가합니다."
  (let ((from-vertex (find from-id (graph-vertices graph) :key #'vertex-id :test #'equal)))
    (when from-vertex
      (push (make-edge :to to-id :label label) (vertex-edges from-vertex)))))


(defun create-dialogue-graph ()
  "간단한 대화 그래프를 생성합니다."
  (let ((g (make-empty-graph)))
    ;; 정점 추가
    (add-vertex g 
		:id 'start 
		:label (make-dialogue 
			:from 'npc1 
			:message "안녕? 무슨 일이야?"))
    (add-vertex g 
		:id 'choice1 
		:label (make-dialogue :from 'npc2 :message "도움이 필요해?" ))
    (add-vertex g 
		:id 'result1
		:label (make-dialogue :from 'npc2 :message "도와줘서 고마워!"))
    (add-vertex g 
		:id 'result2 
		:label (make-dialogue :from 'npc2 :message "알겠어, 좋은 하루 보내!"))
    
    ;; 간선 추가
    (add-edge g 'start 'choice1 "선택지")
    (add-edge g 'choice1 'result1 "네")
    (add-edge g 'choice1 'result2 "아니요")
    
    g))

(defun dialogue/print (dialogue)
  (format nil "~A ~A" 
	  (dialogue-from dialogue)
	  (dialogue-message dialogue)))
	  

(defun print-graph (graph)
  "그래프를 읽기 쉽게 출력합니다."
  (dolist (vertex (graph-vertices graph))
    (format t "Vertex ~a: ~a~%" (vertex-id vertex) (dialogue/print (vertex-label vertex)))
    (dolist (edge (vertex-edges vertex))
      (format t "  -> ~a (Label: ~a)~%" (edge-to edge) (edge-label edge)))))

(defun find-vertex (graph id)
  (let* ((vertices (graph-vertices graph)))
    (remove-if-not #'(lambda (v) 
		       (let* ((vid (vertex-id v)))
			 (equal vid id)))
		   vertices)))

(defun vertex-has-edges-to (vertex to-id)
  (let* ((edges (vertex-edges vertex))
	 (contained-p (find to-id edges :key #'edge-to :test #'string=)))
    contained-p))
					     

(defun find-parent-vertices (graph id)
  "해당 vertex id 를 가지고 있는 모든 vertex를 가져온다."
  (let* ((vertices (graph-vertices graph)))
    (remove-if-not #'(lambda (v) 
		       (vertex-has-edges-to v id))
		   vertices)))
		       
	 
