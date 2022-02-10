type case = J | R | E
type grille = case array array

(* Définit la grille de jeu *)
let grille = ref (Array.make 6 (Array.make 7 E))

(* Définit une fonction qui vérifie si une colonne est pleine *)
let est_pleine (x : int) : bool = (!grille.(0).(x - 1) <> E);; 

(* Définit une fonction qui place un élément de type case dans la grille *)
let rec placer (couleur : case) : int * int =
	print_endline "Placer pion à la colonne : ";
	let rep = read_line() in
	let x_pion_place = ref 0 in
	if rep >= "1" && rep <= "7"
	then (if est_pleine (int_of_string rep) then (print_endline "Colonne pleine ."; placer couleur)
			else 
				begin
					for i = 0 to Array.length !grille - 2 do 
						if !grille.(i).(int_of_string rep) = E && !grille.(i + 1).(int_of_string rep) <> E
						then (!grille.(i).(int_of_string rep) <- couleur; x_pion_place := i);
						(!x_pion_place, int_of_string rep)
					done;
					if !grille.(Array.length !grille - 1).(int_of_string rep) = E
					then (!grille.(Array.length !grille - 1).(int_of_string rep) <- E; x_pion_place := Array.length !grille - 1);
					(!x_pion_place, int_of_string rep)
				end)
	else (print_endline "Colonne non valide ."; placer couleur);;

(* Définit une fonction qui vérifie que l'on puisse continuer à jouer *)
let grille_pleine : bool = 
	let fin = ref true in
	for i = 0 to Array.length !grille.(0) - 1 do 
		if est_pleine (i + 1) = false then fin := false
	done;
	!fin;;

(*Définit une focntion qui vérifie si 4 pions sont alignés en horizontal *)
let horizontal (y : int) (couleur : case) : bool = 
	let total = ref 0 in 
	for i = 0 to Array.length !grille.(y) - 1 do 
		if !grille.(y).(i) = couleur then incr total else total := 0;
		if !total = 4 then true;
	done;
	false;;

(* Définit une focntion qui vérifie si 4 pions sont alignés verticalement *)
let vertical (x : int) (couleur : case) : bool = 
	let total = ref 0 in 
	for i = 0 to Array.length !grille - 1 do 
		if !grille.(i).(x) = couleur then incr total else total := 0;
		if !total = 4 then true;
	done;
	false;;

(* Définit une fonction qui vérifie si 4 pions sont alignés diagonalement ainsi : /*)
let diagonal_plus (x : int) (y : int) (couleur : case) : bool =  
	let i = ref 0 in 
	let total = ref 0 in 
	while (x + !i <= 6 || y + !i <= 5) do 
		if !grille.(y + !i).(x + !i) = couleur then incr total else total := 0;
		if !total = 4 then true;
		incr i
	done;
	false;;

(* Définit une fonction qui vérifie si 4 pions sont alignés diagonalement ainsi : \*)
let diagonal_minus (x : int) (y : int) (couleur : case) : bool =  
	let i = ref 0 in 
	let total = ref 0 in 
	while (x - !i >= 0 || y - !i >= 0) do 
		if !grille.(y - !i).(x - !i) = couleur then incr total else total := 0;
		if !total = 4 then true;
		i := !i - 1
	done;
	false;;

(* Définit une focntion qui renvoie le plus petit élément entre x et y *)
let plus_petit (x : int) (y : int) : int = 	if x > y then x else y

(* Définit deux couple contenant les coordonnées du dernier pion posé dans chaque camp *) 
let r_coor = ref (placer R)
let j_coor = ref (placer J)

(* Définit une fonction qui vérifie s'il existe un alignement de 4 pions *)
(*let alignement (couleur : case) : bool =
	 
	(* Recherche un point de départ à l'analyse de la diagonale / (plus) ou \ (minus) *)
	let for_plus = plus_petit x ((Array.length !grille - 1) - y);
	let for_minus = plus_petit ((Array.length !grille.(0) - 1) - x) ((Array.length !grille - 1) - y)
	if horizontal y couleur || vertical x couleur || (diagonal_plus (x - for_plus) (y + for_plus) couleur)
	|| (diagonal_minus (x + for_minus) (y + for_minus) couleur) then true else false;;*)
