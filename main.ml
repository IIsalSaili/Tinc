open Random

type joueur = {
    mutable pv : int;
    mutable pv_max : int;
    mutable score : int;
    mutable max_life : int;
    mutable life : int;
}

type tiles = {
    mutable type : char;
    mutable position : int;
}

type map = {
    mutable size : int;
}

let affiche_map map = 
     


let hit =
    joueur

let verif_tile = 
    let touche = wait_next_event [Key_pressed] in
    if tile.type = touche then hit

(*let loop = *)
