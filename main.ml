open Curses


let noir = 0
let gris = noir+1
let blanc = gris+1
let rouge = blanc+1
let rouge_clair = rouge+1
let vert = rouge_clair+1
let vert_clair = vert+1
let bleu = vert_clair+1
let bleu_clair = bleu+1


let ncolors = bleu_clair + 1

let cree_couleurs () =
    assert(init_color noir 0 0 0);
    assert(init_color gris 500 500 500);
    assert(init_color blanc 1000 1000 1000);
    assert(init_color rouge 1000 0 0);
    assert(init_color rouge_clair 1000 500 500);
    assert(init_color vert 0 1000 0);
    assert(init_color vert_clair 500 1000 500);
    assert(init_color bleu 0 0 1000);
    assert(init_color bleu_clair 500 500 1000)

let _ =
    let w = initscr () in
    assert(nodelay w true);
    assert(keypad w true);
    assert (start_color ());
    assert (cbreak ());
    assert (noecho ())

let cree_paires () =
    let paires = Array.make_matrix ncolors ncolors 0 in
    let p = ref 10 in
    for i = 0 to ncolors-1 do
        for j = 0 to ncolors-1 do
            assert(init_pair !p i j);
            paires.(i).(j) <- !p;
            incr p
        done
    done;
    paires

let paires = 
    (* cree des couleurs et toutes les paires *)
    cree_couleurs ();
    cree_paires ()

let couleur texte fond =
    attron (A.color_pair paires.(texte).(fond))

(* affiche un pixel *)
let putpixel col x y =
    couleur col col;
    ignore(mvaddch y x (int_of_char ' '))

(* on peut alors dessiner directement *)
let ligne_horiz col x1 x2 y =
    for x = x1 to x2 do
        putpixel col x y
    done

let ligne_vert col x y1 y2 =
    for y = y1 to y2 do
        putpixel col x y
    done

let boite col x1 y1 x2 y2 =
    ligne_horiz col x1 x2 y1;
    ligne_horiz col x1 x2 y2;
    ligne_vert col x1 y1 y2;
    ligne_vert col x2 y1 y2

let _ =
    let w = initscr () in
    assert(nodelay w true);
    assert(keypad w true);
    assert (start_color ());
    assert (cbreak ());
    assert (noecho ())

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

let lose joueur=
    joueur.hp <- 0
    (*Printf.sprintf "Défaite ! Score atteint avant de perdre : %d", joueur.score*)

let win joueur=
    Printf.sprintf "Victoire ! Score atteint : %d", joueur.score

let hit joueur= 
    (*La fonction qui agit quand le joueur marque un point*)
    let ajout = ref 0 in
    joueur.score <- joueur.score + 100 * joueur.score_streak;
    joueur.score_streak <- (joueur.score_streak + 1);
    if joueur.hp >550 then ajout := 0 else ajout := 50;
    joueur.hp <- (joueur.hp + !ajout)

let fail joueur = 
    (*La fonction qui agit quand le joueur fait une erreur*)
    match joueur.hp with
    | x when x >80 -> joueur.hp <- (joueur.hp - 80)
    | _ -> lose joueur;
    joueur.hp <- (joueur.hp - 80)

let modif_touche i= 
    match i with 
    |0 -> 103
    |1 -> 104
    |2 -> 105
    |3 -> 106
    |4 -> 107
    |_ -> 160

let verif_tile tab joueur= 
    let touche = getch () in
    let touch = ref 0 in
    let new_tab =  (String.to_bytes tab) in
    if touche >= 0
    then begin
    for i=0 to Bytes.length(new_tab) - 1 do 
        match tab.[i] with
        |'1' -> if modif_touche i = touche then
         begin
            Bytes.set new_tab i '0';
            touch := 1;
        end
        |_ -> if modif_touche i = touche then touch :=2
    done;
    end;
    if !touch = 1 then hit joueur else fail joueur


let _ =
    let nb_levels = 2 in
    let running = ref true in
    let state = ref 't' in

    let selection = ref 1 in
    let ch = ref 0 in

    
    attroff(A.color);
    (* ----------- main loop ---------- *)
    while !running do
    try
        clear();
        let h, w = get_size () in
     
        
        if !state = 't' then begin

            couleur rouge noir;
            ignore (mvaddstr (h/2-2) (w/2-8) (Printf.sprintf "Tinc main %3d title" !ch));
            couleur blanc noir;
            ignore (mvaddstr (h/2) (w/2-10) (Printf.sprintf "Press space to continue"));
        

        end
        else if !state = 'l' then begin
            couleur rouge noir;
            ignore (mvaddstr (h/2-4) (w/2-8) (Printf.sprintf "Choose your level"));
            
            if !selection = 1 then couleur vert noir else couleur blanc noir;
            ignore (mvaddstr (h/2) (w/2-8) (Printf.sprintf "level 1"));
                
            if !selection = 2 then couleur vert noir else couleur blanc noir;
            ignore (mvaddstr (h/2) (w/2) (Printf.sprintf "level 2"));

            couleur blanc noir;
            boite blanc (h/2-h/4) (w/2-w/4) (w/2-w/4) (h/2-h/4);

        end;
        
        

        Unix.sleepf 0.05;
        ignore(refresh());

        let c = getch () in
        if c >= 0 then begin
            match c with
            | 27 -> running := false;

            | 32 -> if !state = 't' then state := 'l'
            else ()
    (*left*)| 260 -> if !state = 'l' && !selection > 1 then selection := !selection -1
   (*right*)| 261 -> if !state = 'l' && !selection < nb_levels then selection := !selection +1
            | _ -> ()
            
        end;
        if c <> -1 then ch := c;


        
    with e -> begin 
        
        running := false;
        let s = Printexc.to_string e in
        Printf.printf "%s" s;
        end;
    done;
    
    
    endwin ()
    
