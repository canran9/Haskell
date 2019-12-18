data Arbre = VideA | ConsA Int Arbre Arbre

hauteur VideA = 0
hauteur (ConsA Int xs ys) = 1+max (hauteur xs)(hauteur ys)