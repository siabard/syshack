(in-package #:syshack)

(defstruct vertex
  id        ;; 정점의 고유 ID
  label     ;; 정점에 표시할 텍스트
  edges)    ;; 정점에서 나가는 간선의 리스트

(defstruct edge
  to        ;; 연결되는 정점의 ID
  label)    ;; 간선의 레이블 (선택지 등)

(defstruct graph
  vertices) ;; 그래프의 모든 정점을 포함하는 리스트

(defun make-graph ()
  "빈 그래프를 생성합니다."
  (make-graph :vertices '()))

(defun add-vertex (graph id label)
  "그래프에 정점을 추가합니다."
  (push (make-vertex :id id :label label :edges '()) (graph-vertices graph)))

(defun add-edge (graph from-id to-id label)
  "그래프에서 특정 정점 사이에 간선을 추가합니다."
  (let ((from-vertex (find from-id (graph-vertices graph) :key #'vertex-id :test #'equal)))
    (when from-vertex
      (push (make-edge :to to-id :label label) (vertex-edges from-vertex)))))


(defun create-dialogue-graph ()
  "간단한 대화 그래프를 생성합니다."
  (let ((g (make-graph)))
    ;; 정점 추가
    (add-vertex g 'start "NPC1: 안녕? 무슨 일이야?")
    (add-vertex g 'choice1 "NPC2: 도움이 필요해? (1) 네 (2) 아니요")
    (add-vertex g 'result1 "NPC2: 도와줘서 고마워!")
    (add-vertex g 'result2 "NPC2: 알겠어, 좋은 하루 보내!")
    
    ;; 간선 추가
    (add-edge g 'start 'choice1 "선택지")
    (add-edge g 'choice1 'result1 "네")
    (add-edge g 'choice1 'result2 "아니요")
    
    g))

(defun print-graph (graph)
  "그래프를 읽기 쉽게 출력합니다."
  (dolist (vertex (graph-vertices graph))
    (format t "Vertex ~a: ~a~%" (vertex-id vertex) (vertex-label vertex))
    (dolist (edge (vertex-edges vertex))
      (format t "  -> ~a (Label: ~a)~%" (edge-to edge) (edge-label edge)))))

(defun find-vertex (graph id)
  (let* ((vertices (graph-vertices graph)))
    (remove-if-not #'(lambda (v) 
		       (let* ((vid (vertex-id v)))
			 (equal vid id)))
		   vertices)))
