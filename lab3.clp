(defglobal ?*TASK_PRIORITY_1* =500)

(deftemplate animal
    (slot denumire)
    (slot mancare);vegetarian carnivor
    (slot modViata);nocturn diurn
    (slot mediuViata);acvatic semiacvatic terestru aerian
    (slot modalitateReproducere);pui, oua
    (slot zona)
    (slot medieViata)
    (slot putere);0 la ierbivoare 1,2
    )

(assert(animal
        (denumire leu)
        (mancare carnivor)
        (putere 2)))
(assert(animal
        (denumire hiena)
        (mancare carnivor)
        (putere 1)))
(assert(animal
        (denumire caprioara)
        (mancare vegetarian)
        ))
(assert(animal
        (denumire foca)
        (zona polara)
        ))
(assert(animal
        (denumire broasca)
        (mancare vegetarian)
        (modViata diurn)
        (mediuViata semiacvatic)
        (zona mediteraneeana)
        ))
(assert(animal
        (denumire ariciMare)
        (mancare vegetarian)
        (modViata diurn)
        (mediuViata semiacvatic)
        (zona mediteraneeana)
        ))
(assert(animal
        (denumire ursPolar)
        (zona polara)
        ))
(facts)

;afisare zona polara
(defrule antipattern
    ?p <- (animal{zona == polara}
        		(denumire ?name))
    =>
    (printout t "Animale polare " ?name crlf))

(defrule MediuAcvatic
    (declare(salience ?*TASK_PRIORITY_1*))
    ?p <- (animal {mancare == vegetarian && zona == mediteraniana}
        (denumire ?nume))
    =>
    (printout t "Animalul care este vegetarian si traieste in zonca mediteraneana " ?name crlf)
    )

;(mancare == animal1.mancare && mancare == carnivor)

(defrule pradator-prada
    ?animal1 <- (animal)
    ;(bind ?m1 animal1.mancare)
    ?animal2 <- (animal{mancare == animal1.mancare && mancare == carnivor &&
        	(putere < animal1.putere 
            ;animal1.mancare ==vegetarian
            )})
    ?animal3 <- (animal {mancare == carnivor})
    ?animal4 <- (animal {mancare == vegetarian})
    =>
    (printout t  "Pradatorul :" ?animal1.denumire " si prada " ?animal2.denumire crlf)
    (printout t  "Pradatorul :" ?animal3.denumire " si prada " ?animal4.denumire crlf)
    
    )





(deftemplate note
    (slot matematica (type INTEGER))
    (slot informatica (type INTEGER))
    (slot fizica (type INTEGER))
    (slot chimie (type INTEGER))
    (slot limbaRomana (type INTEGER))
    (slot limbaEngleza (type INTEGER))
    (slot limbaGermana (type INTEGER))
    (slot economie (type INTEGER))
    (slot psihologie (type INTEGER))
    )
(assert (note(matematica 0)(informatica 0)(fizica 0)(chimie 0)(limbaRomana 0)
        (limbaEngleza 0) (economie 0) (psihologie 0)))

(defrule citeste
    ?id <- (note(matematica 0)(informatica 0)(fizica 0)(chimie 0)(limbaRomana 0)
        (limbaEngleza 0) (economie 0) (psihologie 0) )
    =>
    (printout t "Notele elevului: " crlf)
    (printout t "matematica: ")
    (bind ?m (read))
    (printout t "informatica: ")
    (bind ?i (read))
    (printout t "fizica: ")
    (bind ?f (read))
    (printout t "chimie: ")
    (bind ?c (read))
    (printout t "limba Romana: ")
    (bind ?lr (read))
    (printout t "limba Germana: ")
    (bind ?lg (read))
        (printout t "limba engleza: ")
    (bind ?le (read))
        (printout t "economie: ")
    (bind ?e (read))
        (printout t "psiholoie: ")
    (bind ?p (read))
    (retract ?id)
    (assert (note(matematica ?m)(informatica ?i)(fizica ?f)(chimie ?c)(limbaRomana ?lr)
        (limbaEngleza ?le) (economie ?e) (psihologie ?p)))
    )

(defrule FacultateMateInfo
    (note(matematica ?m & :(>= ?m 8))(informatica ?i & :(>= ?i 8))
        (fizica ?f & :(>= ?f 8)))
        =>
        (printout t "Studentul poate sa isi depuna dosarul la matematica si informatica" crlf))

(deftemplate Zbor(slot plecare)(slot sosire) (slot distanta))
(assert( Zbor (plecare NewYork)(sosire Chicago)(distanta 900)))
(assert( Zbor (plecare Chicago)(sosire Denver)(distanta 1200)))
(assert( Zbor (plecare NewYork)(sosire LosAngeles)(distanta 800)))
(assert( Zbor (plecare LosAngeles)(sosire Chicago)(distanta 500)))
(assert( Zbor (plecare Toronto)(sosire NewYork)(distanta 1400)))
(assert( Zbor (plecare Chicago)(sosire Toronto)(distanta 2500)))

(defrule NY
    ;?sosore <-(Zbor(sosire))
    ;?distanta <- (Zbor(distanta))
    (Zbor (plecare NewYork)(sosire ?sosire)(distanta ?distanta))
    =>
    (printout t "Zborurile care pleaca din NY la: " ?sosire " distanta " ?distanta crlf)
    )

(defrule constrangere
    ;?sosire <- (Zbor(sosire)) 
    ;?distanta <- (Zbor(distanta))
    (Zbor (plecare ?plecare)(sosire ?sosire)(distanta ?distanta)
        {distanta > 800 && distanta < 2200})
    =>
    (printout t "Zborurile care pleaca din " ?plecare " la "?sosire" distanta " ?distanta crlf )
)