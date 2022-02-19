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

let _ = Random.init (int_of_float (Unix.gettimeofday ()))

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
    mutable state : char;
}

let player hp score= {hp = hp; hp_max=hp; score = score; score_streak = 0; state = 'a'}

type particule = {
    mutable x : int;
    mutable y : int;
    mutable speed : int;
    mutable color : int;
    mutable compteur : int;
}

let particule x speed col = { x = x; y = 9999; speed = speed; color = col; compteur = 0}

let tick_particules (particules: particule array ref) =
    let h,w = get_size() in
    for i = 0 to Array.length !particules - 1 do
        !particules.(i).compteur <- !particules.(i).compteur +1;

        if !particules.(i).compteur >= !particules.(i).speed then begin
            !particules.(i).compteur <- 0;
            !particules.(i).y <- !particules.(i).y +1;

            if !particules.(i).y > h then begin
                !particules.(i).y <- 0;
                !particules.(i).x <- (Random.int 9999) mod w;
     (*couleur*)!particules.(i).color <- ((Random.int 9999) mod (ncolors-2)) +1;
                
            end;
        end;
        putpixel !particules.(i).color !particules.(i).x !particules.(i).y;
    done

let affichage_end_level result joueur = 
    let h, w = get_size () in
    if result = "lose" then begin
        couleur rouge noir;
        ignore (mvaddstr (h/2) (w/2-18) (Printf.sprintf "YOU LOSE, final score : \n well, it doesn't really matter you lost anyway-"));
    end
    else begin
        couleur vert noir;
        ignore (mvaddstr (h/2) (w/2-8) (Printf.sprintf "YOU WON, final score : %d" joueur.score))
    end 

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

let manual_tri tab =
    match tab with
    |t::q -> if t = "00000" then q else tab
    |[] -> []

let hit (joueur : joueur ref) = 
    (*La fonction qui agit quand le joueur marque un point*)
    let ajout = ref 0 in
    !joueur.score <- !joueur.score + 100 * !joueur.score_streak;
    !joueur.score_streak <- (!joueur.score_streak + 1);
    if !joueur.hp >550 then ajout := 0 else ajout := 50;
    !joueur.hp <- (!joueur.hp + !ajout)
    (*manual_tri tab*)

let fail (joueur : joueur ref) =
    (*La fonction qui agit quand le joueur fait une erreur*)
    begin
      if !joueur.hp>80 then !joueur.hp <- (!joueur.hp - 80) 
      else
      !joueur.state <- 'm';
     end

let touche_valide touche : bool =
    match touche with
    | 103 -> true
    | 104 -> true
    | 106 -> true
    | 107 -> true
    | 108 -> true
    | _   -> false

let indice_of_touche touche : int =
    match touche with
    | 103 -> 0
    | 104 -> 1
    | 106 -> 2
    | 107 -> 3
    | 108 -> 4
    | _   -> 999


let modif_touche i= 
    match i with 
    |0 -> 103
    |1 -> 104
    |2 -> 106
    |3 -> 107
    |4 -> 108
    |_ -> 160

let verif_tile (tab : string list) (joueur : joueur ref) touche : string list= 
    (*
    let action_valide = ref false in 
    let tete =  (String.to_bytes (List.hd tab)) in
    *)

    let ligne_finie = ref false in
    let tete = ref (List.hd tab) in

    if touche > 0 then begin

        let tete_bytes = ref (String.to_bytes (List.hd tab)) in
        let valide = ref false in
        if touche_valide touche then begin
            let i = indice_of_touche touche in
                
            if !tete.[i] = '1' then begin

                valide := true;
                Bytes.set !tete_bytes i '0';

            end else begin
                valide := false; (*inutile mais plus clair*)
            end;
        end else begin
            valide := false; (*inutile mais plus clair*)
        end;
        
        if !valide then begin
            hit joueur;
            tete := Bytes.to_string !tete_bytes;
            if !tete = "00000" then ligne_finie := true;

        end else
            fail joueur;
            
        (*
        for i=0 to Bytes.length(tete) - 1 do 

            match (List.hd tab).[i] with
            |'1' -> if modif_touche i = touche then
            begin
                Bytes.set tete i '0';
                action_valide := true;
                next := true;
            end
            |_ ->begin
                velide := false;
                next :=true;
            end
        done;
        *)
    end;
    (*
    let remise = Bytes.to_string new_liste in
    let new_tab = tab @ remise in
    if !touch = 0 then tab
    else if !touch = 1 then hit joueur new_tab 
    else fail joueur new_tab
    *)
    if !ligne_finie then List.tl tab
    else [!tete] @ List.tl tab

let affichage_tab tab selection =
    let size = List.length tab in
    couleur bleu noir;
    clear();
    let h, w = get_size () in
    if size > 6 then begin
        for i= 0 to 4 do
            let hd = List.nth tab i in
            ignore (mvaddstr (6+h/2-2*i) (5+w/2-8) (Printf.sprintf "%s" hd));
        done;
        end;
    ignore (mvaddstr (h/2-10) (w/2-4) (Printf.sprintf "Level %d" selection))

    let affichage_hp joueur = 
        let h, w = get_size () in
        couleur rouge vert;
        ignore (mvaddstr (h/10) (w/10) (Printf.sprintf "HP : %d" joueur.hp))

let print_title () =
    ligne_horiz rouge 2 15 7;
    ligne_vert rouge 8 8 15;
    ligne_vert rouge 9 8 15
(* ------------------------------------------ MAIN FUNCTION --------------------------------- *)

let _ =
    (* variables generales *)
    let h, w = get_size () in
    ignore (h);
    let nb_levels = 5 in
    let running = ref true in
    let state = ref 't' in
    let touche = ref 0 in

    let joueur = ref (player 549 0) in
        !joueur.hp <- !joueur.hp +1;

    let selection = ref 1 in

    let particules = ref [|(particule (Random.int w) (Random.int 7) ((Random.int (ncolors -1))+1))|] in

    (* variables temporaires *)
    let ch = ref 0 in
    
    (* variables level *)
    let in_game = ref false in

    let tab = ref [] in
    let result = ref "lose" in
    (*let osef = ref true in*)

    let com_particules = ref 0 in
    attroff(A.color);
    (* ----------- main loop ---------- *)
    while !running do
    try
        clear();
        let h, w = get_size () in

        if !com_particules < w/2 then begin
            particules := Array.append !particules [|(particule (Random.int w) (Random.int 7) ((Random.int (ncolors -1))+1))|];
            com_particules := !com_particules +1;
        end;
        if not !in_game then tick_particules particules;

        if !state = 't' then begin

            print_title ();
            couleur rouge noir;
            ignore (mvaddstr (h/2-2) (w/2-8) (Printf.sprintf "Tinc main %3d title" !ch));
            couleur blanc noir;
            ignore (mvaddstr (h/2) (w/2-10) (Printf.sprintf "Press space to continue"));
        

        end
        else if !state = 'l' then begin
            couleur rouge noir;
            ignore (mvaddstr (h/2-4) (w/2-8) (Printf.sprintf "Choose your level"));

            for i = 1 to nb_levels do
                if !selection = i then couleur vert noir else couleur blanc noir;
                ignore (mvaddstr (h/2) (w/2-(!selection)*8 - (nb_levels-i*8) +1 ) (Printf.sprintf "level %d" i));
            done;


        end
        else if !state = 'g' then begin
            if not !in_game then begin

                tab := load_level !selection;
                (*tab := ["10000";"01000";"10000";"00100";"00001";"00010";"01000"];*)
                in_game := true;

            end else begin
                (* level loop *)
                if !joueur.state = 'a' then begin
                    try
                        (*
                        if !osef = true then begin
                        affichage_tab !tab !selection;
                        osef:= false;
                        end;
                        tab := verif_tile !tab joueur !touche;
                        affichage_tab !tab !selection;
                        *)
                        tab := verif_tile !tab joueur !touche;
                        affichage_tab !tab !selection;
                        affichage_hp !joueur;
                    with Failure a -> begin 
                        affichage_end_level !result !joueur; 
                        ignore a;
                        end
                end else begin
                    affichage_end_level "lose" !joueur;
                end;
                
            end;
        end;
        
        

        Unix.sleepf 0.05;
        ignore(refresh());

        touche := getch ();
        if !touche >= 0 then begin
            match !touche with
            | 27 -> running := false;
  (*espace*)| 32 -> if !state = 't' then state := 'l'
               else if !state = 'l' then state := 'g' 
    (*left*)| 260 -> if !state = 'l' && !selection > 1 then selection := !selection -1
   (*right*)| 261 -> if !state = 'l' && !selection < nb_levels then selection := !selection +1
   (*enter*)| 10 -> if !state = 'l' then state := 'g';
            | _ -> ()
            
        end;
        if !touche <> -1 then ch := !touche;


        
    with e -> begin 
        running := false;
        Printf.printf "%s\n" (Printexc.to_string e);
        end;
    done;
    
    
    endwin ()
    