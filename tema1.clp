(deftemplate ArcaluiNoe (multislot animal) (multislot gen))
(assert (ArcaluiNoe (animal albina) (gen mascul)))
(assert (ArcaluiNoe (animal albina) (gen femela)))
(assert (ArcaluiNoe (animal soarece) (gen mascul)))
(assert (ArcaluiNoe (animal soarece) (gen femela)))
(assert (ArcaluiNoe (animal veverita) (gen mascul)))
(assert (ArcaluiNoe (animal veverita) (gen femela)))
(assert (ArcaluiNoe (animal pisica) (gen mascul)))
(assert (ArcaluiNoe (animal pisica) (gen femela)))
(assert (ArcaluiNoe (animal caine) (gen mascul)))
(assert (ArcaluiNoe (animal caine) (gen femela)))
(assert (ArcaluiNoe (animal leu) (gen mascul)))
(assert (ArcaluiNoe (animal leu) (gen femela)))
(assert (ArcaluiNoe (animal vaca) (gen mascul)))
(assert (ArcaluiNoe (animal vaca) (gen femela)))
(assert (ArcaluiNoe (animal elefant) (gen mascul)))
(assert (ArcaluiNoe (animal elefant) (gen femela)))
(facts)

(defrule regula1
    (ArcaluiNoe (animal $?x) (gen femela))
    =>
    (printout t "Animalul " $?x " este femela " crlf))
(run)