open Curses

type joueur = {
    mutable hp : int;
    mutable hp_max : int;
    mutable score : int;
    mutable score_streak : int;
}

let actual = ref 0

let player hp score= {hp = hp; hp_max=hp; score = score; score_streak = 0}

let read =  
    "10000"
    
let load_level (level : int) : string list=
    
    let nom_dossier = ref "levels/level" in
    nom_dossier := !nom_dossier ^ (string_of_int level);
    nom_dossier := !nom_dossier ^ ".txt";

    let lignes = ref [] in
    let file = open_in !nom_dossier in

    try
        while true do
            lignes := input_line file :: !lignes
        done;
        !lignes
    with End_of_file -> close_in file;
    
    List.rev !lignes;;

(*let read_next =*) 

    (*Ici faudra mettre toute la partie du code qui lit la prochaine ligne et 
    modifie la liste map *)

let lose joueur=
    Printf.sprintf "Défaite ! Score atteint avant de perdre : %d", joueur.score

let win joueur=
    Printf.sprintf "Victoire ! Score atteint : %d", joueur.score

let hit joueur= 
    (*La fonction qui agit quand le joueur marque un point, oui c'était assez évident *)
    let ajout = ref 0 in
    joueur.score <- joueur.score + 100 * joueur.score_streak;
    joueur.score_streak <- (joueur.score_streak + 1);
    if joueur.hp >550 then ajout := 0 else ajout := 50;
    joueur.hp <- (joueur.hp + !ajout)

(*let fail joueur= 
    (*Soit le joueur a encore des hp et il en perd, soit il a perdu*)
    if joueur.hp<=80 then lose else joueur.hp <- joueur.hp-80 else lose*)

let fail joueur = 
    joueur.hp <- (joueur.hp - 80)

let modif_touche i= 
    match i with 
    |0 -> 'g'
    |1 -> 'h'
    |2 -> 'j'
    |3 -> 'k'
    |4 -> 'l'
    |_ -> '@'

let verif_tile tab joueur= 
    let touche = getch () in
    let touch = ref 0 in
    let new_tab =  (String.to_bytes(tab)) in
    if touche >= 0
    then begin
    for i=0 to Bytes.length(new_tab) - 1 do 
        (*if tab.(actual).(i) = touche then hit else fail*)
        match tab.[i] with
        |'1' -> if modif_touche i = char_of_int(touche) then
         begin
            Bytes.set new_tab i '0';
            touch := 1;
        end
        |'0' -> if modif_touche i = char_of_int(touche) then touch :=2
        |_ -> touch := 2
    done;
    end;
    if !touch = 1 then hit joueur else fail joueur

