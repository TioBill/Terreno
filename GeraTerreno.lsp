(alert "Use de preferencia o UCS esta setado para WORLD antes de comecar! Obrigado pela compreensao.")

(defun c:GeraTerreno ( / *error* pol drawLine intersections acadObj aDoc modelSpace polyline polylineElevation curvasDeNiveis drawPosition yScale tamanhoTexto)
  (defun *error* (msg)
    (or 
      (wcmatch (strcase msg t) "*break,*cancel*,*exit*") 
      (alert (strcat "ERROR: " msg "**"))
    )
  )
  
  (defun intersections ( polyline lista drawPosition yScale modelSpace tamanhoTexto / i currentObj elevation lst currentDistance pointPosition)
    
   (repeat (setq i (sslength lista))
    
    (setq 
      currentObj (vlax-ename->vla-object (ssname lista (setq i (1- i))))
    )
     
    (if (eq "LWPOLYLINE" (cdr (assoc 0 (entget (vlax-vla-object->ename currentObj)))))
      (setq elevation (vlax-get currentObj 'Elevation))
      (setq elevation (caddr (vlax-get currentObj 'ControlPoints)))
    )
     
    ;; Troca a elevacao do polyline para a elevacao da curva de nivel atual
    (vla-put-elevation polyline elevation)
     
    (if (and 
          (vlax-method-applicable-p polyline 'intersectwith) 
          (vlax-method-applicable-p currentObj 'intersectwith)
          (setq lst (vlax-invoke polyline 'intersectwith currentObj acExtendNone))
        )
        (progn
          ;; Change the current elevation to its previous one
          
          (repeat (/ (length lst) 3)
            (setq currentDistance (vlax-curve-getdistatpoint polyline (list (car lst) (cadr lst) (caddr lst))))

            (setq pointPosition
              (list 
                (+ (car drawPosition) currentDistance)
                (+ (cadr drawPosition) (* elevation yScale))
                (caddr drawPosition)
              )
            )
            
            (setq pointsList (append pointsList (list pointPosition)))
            
            (vla-addtext 
              modelSpace
              (rtos elevation 2 3)
              (vlax-3d-point (trans (list (- (car drawPosition) (* 10 tamanhoTexto)) (cadr pointPosition) (caddr pointPosition)) 1 0))
              tamanhoTexto
            )
            
            (vla-put-rotation 
              (vlax-ename->vla-object (entlast)) 
              (angle 
                (trans '(0 0) 1 0)
                (trans '(0 1) 1 0)
              )
            )
            
            (vla-put-alignment (vlax-ename->vla-object (entlast)) acAlignmentMiddleCenter)
            
            (setq lst (cdddr lst))
          )
        )
    )
   )
  )
  
  (defun drawLine (lista / newList i j vlaxPoints)
    (setq lista (vl-sort lista (function (lambda (e1 e2) (< (car e1) (car e2))))))
    
    (setq newList (list))
    
    (setq i 0)
    
    (repeat (length lista)
      (setq j 0)
      
      (repeat (length (nth i lista))
        (setq newList 
               (append 
                 newList 
                 (list 
                   (nth 
                     j
                    (trans (nth i lista) 1 0)
                    )
                 )
               )
        )
        
        (setq j (1+ j))
      )
      
      (setq i (1+ i))
    )
    
    (setq vlaxPoints 
           (vlax-make-safearray 
             vlax-vbDouble 
             (cons 0 (- (length newList) 1))
           )
    )

    (vlax-safearray-fill vlaxPoints newList)
                       
    (vla-addpolyline modelSpace vlaxPoints)
  )
  
  (setq
    acadObj (vlax-get-acad-object)
    aDoc (vla-get-activedocument acadObj)
    modelSpace (vla-get-modelspace aDoc)
  )
  
  (setq polyline 
         (vlax-ename->vla-object 
            (progn
              (while (null (setq pol (car(entsel "Selecione a polyline: ")))))
              pol
            )
         )
  )

  (setq polylineElevation (vlax-get polyline 'Elevation))
  
  (alert "Selecione as curvas de niveis: (2D Polyline ou SPLINES apenas)\n")
  (setq curvasDeNiveis 
    (ssget 
      '(
        (-4 . "<OR")
          (0 . "LWPOLYLINE") 
          (0 . "SPLINE")
        (-4 . "OR>")
      )
    )
  )
  
  (setq curvasDeNiveis (ssdel (vlax-vla-object->ename polyline) curvasDeNiveis))

  (setq drawPosition (getpoint "Selecione um ponto base: "))
  
  (or
    (setq yScale (getreal "Digite uma escala Y [1]: "))
    (setq yScale 1)
  )
  
  (initget (+ 1 2 3))
  (setq tamanhoTexto (getreal "Tamanho do texto: "))
    
  (setq pointsList (list))
  
  (intersections polyline curvasDeNiveis drawPosition yScale modelSpace tamanhoTexto)
  (drawLine pointsList)
  
  (command "_zoom" "_o" (entlast) "")

  (vla-put-elevation polyline polylineElevation)
)

(alert "Lisp carregada! Digite \"GeraTerreno\"")


