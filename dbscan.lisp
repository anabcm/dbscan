;CENTRO DE INVESTIGACION EN COMPUTACION. IPN. MARZO 2014
;ISC.ANA BERTHA CRUZ MARTINEZ
;DBSCAN-Density-based spatial clustering of applications with noise
;==============Funcion Agrupa======================================
;Funncion de agrupamiento en formato universal, usa una lista donde recibe los parametro m y ejecuta
; un ejemplo de ejecucion es (agrupa "nombredelarchivo" :dbscan '(1.0 8))
(defun agrupa (path &key ((:dbscan  lista1) nil))
	(ejecuta path (car lista1) (nth 1 lista1)); toma los valores de la lista para m y e
)
;===========================EJECUTA================
;Manda llamar las funciones auxiliares para la carga de archivos
;Inicializa algunas listas que seran usadas como auxiliares y crea el array para señalar el tipo
;de patron(central, frontera o ruido)
(defun ejecuta(p e m )
	(setq numPatrones 0)(setq numRasgos 0)(setq numClases 0)
	(setq *Patrones* nil)
	(setq E e)(setq M m)
	(setq grupos nil)
	(leeArchivo p)
	(setq carP (make-array (list numPatrones 2)))
	(creaMatriz numPatrones)
	(dbscan E M)
)
;==============================Funcion de la distancia-sintactica================================================
;Funcion de distancia para los patrones del conjunto tic-tac-toe
;Realiza la sumatoria de cada rasgo, que son evaluados con la funcion auxiliar funcion-rasgo
;Su Rango es [0,8], donde seria 8 si son completamente diferentes
(defun distancia-sintactica (patron1 patron2)
	(setq suma(+
		(funcion-rasgo(nth 0 patron1 )(nth 0 patron2 ))
		(funcion-rasgo(nth 1 patron1 )(nth 1 patron2 ))
		(funcion-rasgo(nth 2 patron1 )(nth 2 patron2 ))
		(funcion-rasgo(nth 3 patron1 )(nth 3 patron2 ))
		(funcion-rasgo(nth 4 patron1 )(nth 4 patron2 ))
		(funcion-rasgo(nth 5 patron1 )(nth 5 patron2 ))
		(funcion-rasgo(nth 6 patron1 )(nth 6 patron2 ))
		(funcion-rasgo(nth 7 patron1 )(nth 7 patron2 ))
		(funcion-rasgo(nth 8 patron1 )(nth 8 patron2 ))
	)
	)
)
;funcion de evaluacion para cada rasgo, evalua cada valor del tic-tac-toe
;si son iguales la diferencia sera 0
;si son (o,x) entonces son diferentes y el valor sera 1
;si tiene un valor b, quiere decir que existe la posibilidad de tirar y se evalua con la mita de la distancia (0.5)
(defun funcion-rasgo (x y )	
	(cond( (equal x y ) 0)
	((or (and (String-equal x "X") (String-equal y "O")) (and (String-equal x "O") (String-equal y "X"))) 1)
	((or (and (String-equal x "X") (String-equal y "B")) (and (String-equal x "B") (String-equal y "X"))) 0.5)
	((or (and (String-equal x "O") (String-equal y "B")) (and (String-equal x "B") (String-equal y "O"))) 0.5)
	)
	
)
;==============================Carga de archivo==================================================================
;Lee el archivo y lo guarada en una lista llamada *Patrones*
(defun leeArchivo (path)
 (setq *Patrones* nil)
    (with-open-file (stream path)
      (setq numPatrones (read stream nil nil))
      (setq numrasgos (read stream nil nil))
      (setq numclases (read stream nil nil))
      (read-line stream nil nil)
      (dotimes (i numPatrones)
        (setq patroni (read-from-string  (read-line stream nil nil)))
        (setq *Patrones* (cons patroni *Patrones*))  
        )
      )	 
  )
;===================================Matriz de distancia entre patrones==========================================
;Calcaula la matriz de distancia entre patrones y la guarda en una tabla hash llamada Distancia
 (defun creaMatriz (n)
	(let (distanciaj)
      (dotimes (i n)
	   (dotimes (j n)
		   (setq distanciaj (distancia-sintactica (nth i *Patrones* )(nth j *Patrones*)))
		   (escribe-valorDis i j distanciaj)   
	   )
	  )
    )
)
;==============Manejo de tabla hash de distancias======================================================
;Funciones auxilires para manejar la tabla hash como tringular
(setq Distancia (make-hash-table :test #'equal))
;Funcion de escritura en el archivo que maneja los indices como tabla triangular
(defun escribe-valorDis (renglon columna valor)
	(setq llave (valida renglon columna))
	(if (equal llave nil)
	(setf (gethash llave Distancia ) 0)
	(setf (gethash llave Distancia ) valor))
)
;Lee de la tabla hash, manejandola como tabla triangular
(defun lee-valorDis (renglon columna)
	(setq llave (valida renglon columna))
	(gethash llave Distancia)
)
;Valida los indices para la lectura-escritura en la tabla hash, simulado que es la matriz completa.
(defun valida (renglon columna)
	(cond ((> renglon columna) (list renglon columna))
		((equal renglon columna) nil)
		((< renglon columna) (list columna renglon))
	)
)
;===============================dbscan===========================================================
;========================Centrales========
;Construye una lista con los patrones centrales que cumplen con el numero minimo de vecinos
;que estan a a una distancia m
(defun centrales (m e) 
(setq central nil)
	(dotimes (i numPatrones)
	(if (>= (cardinalidad i e) m); si la cardinalidad calculada para el patron es mayor o igual a m se agrega a la lista de centrales
		(setq central (cons i central))
	))
)
;===============================Inicia arreglo============================
;inicializa el arreglo de datos que almacenará que tipo de patrón (central,frontera o ruido)
;la columna cero tiene la densidad, la columna 1 tiene el tipo de patron (central 1,frontera 2,ruido 0)
(defun inicia-marcas(e)
	(dotimes (i numPatrones)
			(setq densi (cardinalidad i e))
			(setf (aref carP i 0) densi);cambia las densidad para cada elemento
			(setf (aref carP i 1) 0);los inicia a todos como ruido
	)
)
;==========================cardindalidad==============================
;Calcula la cardinalida del patron i, si la distancia que existe del patron i a cada patron j es menor o igual a e, se incrementa el contador
(defun cardinalidad(indicei e)
(setq c 0)
	(dotimes (j numPatrones)
		(if (<= (lee-valorDis indicei j) e)
			(setq c(+ c 1))
		)
	)
	(setq c c)
)
;=================================Vecindad=============================
;Del patron i determina quienes son los vecinos mas proximos al patron i acorde a la distancia e
(defun vecindad (indicei e)
(setq listag nil)
	(dotimes (j numPatrones)
		(if (and (<= (lee-valorDis indicei j) e) (equal (aref carP j 1)0))
			(setq listag (cons j listag))			
		)
	)
	(setq listag listag)
)
;=========================DBSCAN=================================================================
;Aqui se implementaron las generalidades del algoritmo DBSCAN
(defun dbscan (e m)
(setq grupos nil);Es una lista de los grupos generados
(setq grupito nil);contiene la vecindad de los patrones centrales
(setq grupo-actual nil);contiene los elementos que se acumulan hasta cerrar un grupo y que todos sean señalados como central o ruido
(setq vecindadactual nil)
(inicia-marcas e); inicializa todos los patrones como ruido y calcula la cardinalidad para cada patron
(centrales m e);calcula los patrones centrales
;iniciara ciclo
(if (not(equal central nil));si no hay centrales todos los patrones se vuelven ruido
	(loop do ;este ciclo es para determinar cada grupo
	(setq grupo-actual nil) 
	(setq central-actual (car central));toma el primer patron central para agrupar
	(setq central (rest central)); guarda el resto de los centrales
	(setq vecindadactual (vecindad central-actual e)) ;calcula la vecindad de lel central actual
	(setf (aref carP central-actual 1) 1); lo marca como central en la tabla
	(setq grupo-actual (remove-duplicates(append grupo-actual vecindadactual))); forma el grupo actual con los elementos existentes en el grupo y la nueva vecindad
	(loop do  ;este ciclo marca los siguientes patrones como centrales o frontera
		(setq grupoaux nil)
		(dotimes (i (length grupo-actual)); para cada elemento del grupo actual se revisa si no ha sido marcadp
			(if (and (>= (aref carP (nth i grupo-actual) 0) m)  (equal (aref carP (nth i grupo-actual) 1) 0))
					(verdad (nth i grupo-actual) e ); si no se marco y es central,se calcula si vecindad 
					(setf (aref carP (nth i grupo-actual) 1) 2); si no es central se marca como frontera						
			)
			(setq grupoaux(append grupoaux grupito))
		)
		(setq grupo-actual (remove-duplicates(append grupo-actual grupoaux)));construye el grupo actual
		(setq c (reduce #'+ (mapcar #'(lambda (x) (if (equal(aref carP x 1) 0) 1 0)) grupo-actual))); revisa si todos fueron marcados
	while(not (equal c 0)); mientras no esten marcados todos lo elementos del grupo se repite el ciclo
	)
	(setq grupos(cons grupo-actual grupos ))
	while(not(equal central nil)); mientras existan centrales por revisar se repite el ciclo
	)
	)
	(ruido); todos los patrones que no se señalaron como central o frontera se toman como ruido
	(imprime); imprime la salida del algoritmo

)
;si resulto que es un patron central se marca como central, se calcula la vecidad del central y se quita de la lista de centrales
(defun verdad(x e)
	(setf (aref carP x 1) 1)
	(setq grupito (vecindad x e)) 
	;(print x)
	(setq central(remove x central	))
)
;enlista todos los elementos que quedaron como ruido en el arreglo
(defun ruido ()
(setq rdo nil)
	(dotimes (i numPatrones)
		(if (equal (aref carP i 1) 0)
			(setq rdo (cons i rdo))
		)
	)
)
;imprime la salida de los diferentes grupos de forma ordenada
(defun imprime()
	(dotimes (i (length grupos))
		(print "Grupo ") (princ i)(print (sort (nth i grupos) #'<	))
	)
	(print "Ruido ")(print rdo)
	(setq rdo nil)
)
